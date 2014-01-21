-module(lgx).

-export([compile/1]).
-export([compile/2]).
-export([execute/3]).
-export([apply/3]).
-export([apply/4]).

-record(req, {
  ref,
  forms,
  mapfun,
  context,
  completed = 0,
  endname,
  values,
  pending = 0,
  changed = 0,
  required
}).

compile(Defs) ->
  [Def|_] = Defs,
  Name = erlang:element(1, Def),
  compile(Defs, Name).
compile(Defs, Start) ->
  Normalized = normalize_defs(Defs, []),
  StartIndex = index_by_name(Start, Normalized),
  Optional = lookup_conds(Normalized, Normalized, 0),
  {Expanded, Required} = expand_defs(Normalized, bnot Optional, Normalized, []),
  Filtered = removed_unused(Expanded, StartIndex),
  {ok, {Filtered, StartIndex, Required}}.

execute({Forms, End, Required}, MapFun, Context) ->
  execute(#req{ref = erlang:make_ref(),
               forms = Forms,
               mapfun = MapFun,
               context = Context,
               endname = End,
               required = Required}).

apply(Defs, MapFun, Context) ->
  {ok, Forms} = compile(Defs),
  execute(Forms, MapFun, Context).
apply(Defs, Start, MapFun, Context) ->
  {ok, Forms} = compile(Defs, Start),
  execute(Forms, MapFun, Context).

%% compiler.

normalize_defs([], Acc) ->
  Acc;
normalize_defs([Def|Defs], Acc) ->
  Normalized = normalize_def(Def, length(Acc)),
  normalize_defs(Defs, [Normalized|Acc]).

normalize_def({Name, '$cond', Cond, A, B}, Index) ->
  {Name, bitindex(Index), {Cond, A, B}, '$cond'};
normalize_def({Name, Mod, Fun, Args}, Index) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
  {Name, bitindex(Index), {Mod, Fun, Args}, false};
normalize_def({Name, Mod, Fun, Args, spawn}, Index) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
  {Name, bitindex(Index), {Mod, Fun, Args}, spawn};
normalize_def(Fun, _) ->
  erlang:error({invalid_fun, Fun}).

lookup_conds([], _, Optional) ->
  Optional;
lookup_conds([{_, _, {_, A, B}, '$cond'}|Rest], Forms, Optional) ->
  Optional2 = Optional bor index_by_name(A, Forms) bor index_by_name(B, Forms),
  lookup_conds(Rest, Forms, Optional2);
lookup_conds([_|Rest], Forms, Optional) ->
  lookup_conds(Rest, Forms, Optional).

index_by_name(Name, Forms) ->
  case lists:keyfind(Name, 1, Forms) of
    false ->
      erlang:error({not_found, Name});
    {_, Index, _, _} ->
      Index
  end.

expand_defs([], Required, _, Expanded) ->
  {Expanded, Required};
expand_defs([{_, Index, {Cond, A, B}, '$cond'}|Rest], Required, Forms, Expanded) ->
  Dep = index_by_name(Cond, Forms),
  AI = index_by_name(A, Forms),
  BI = index_by_name(B, Forms),
  Required2 = Required bor Dep,
  Expanded2 = [{Index, {Dep, AI, BI}, '$cond', Dep}|Expanded],
  expand_defs(Rest, Required2, Forms, Expanded2);
expand_defs([{_, Index, {M, F, A}, Spawn}|Rest], Required, Forms, Expanded) ->
  {Args, Deps} = replace_args(A, Forms, [], 0),
  Required2 = Required bor Deps,
  Expanded2 = [{Index, {M, F, Args}, Spawn, Deps}|Expanded],
  expand_defs(Rest, Required2, Forms, Expanded2).

replace_args([], _, Args, Deps) ->
  {lists:reverse(Args), Deps};
replace_args([{'$exec', Name}|Rest], Forms, Args, Deps) ->
  Dep = index_by_name(Name, Forms),
  Deps2 = Deps bor Dep,
  Args2 = [{'$exec', Dep}|Args],
  replace_args(Rest, Forms, Args2, Deps2);
replace_args([Arg|Rest], Forms, Args, Deps) when is_list(Arg) ->
  {Arg2, Deps2} = replace_args(Arg, Forms, [], Deps),
  replace_args(Rest, Forms, [Arg2|Args], Deps2);
replace_args([Arg|Rest], Forms, Args, Deps) when is_tuple(Arg) ->
  List = tuple_to_list(Arg),
  {Arg2, Deps2} = replace_args(List, Forms, [], Deps),
  Arg3 = list_to_tuple(Arg2),
  replace_args(Rest, Forms, [Arg3|Args], Deps2);
replace_args([Arg|Rest], Forms, Args, Deps) ->
  replace_args(Rest, Forms, [Arg|Args], Deps).

removed_unused(Forms, Start) ->
  case filter_unused(Forms, Forms, Start, []) of
    Forms ->
      Forms;
    Filtered ->
      removed_unused(Filtered, Start)
  end.

filter_unused([], _, _, Acc) ->
  lists:reverse(Acc);
filter_unused([{I, _, _, _} = Def|Rest], Forms, Start, Acc) ->
  Acc2 = case is_needed(I, Forms, Start) of
    true ->
      [Def|Acc];
    false ->
      Acc
  end,
  filter_unused(Rest, Forms, Start, Acc2).

is_needed(I, _, I) ->
  true;
is_needed(_, [], _) ->
  false;
is_needed(I, [{_, {_, I, _}, '$cond', _}|_], _) ->
  true;
is_needed(I, [{_, {_, _, I}, '$cond', _}|_], _) ->
  true;
is_needed(I, [{_, _, _, Deps}|_], _) when I band Deps =:= I ->
  true;
is_needed(I, [_|Rest], Start) ->
  is_needed(I, Rest, Start).

%% runtime.

execute(Req) ->
  case digest(Req#req{forms = []}, Req#req.forms) of
    %% We're done!
    {ok, Value} ->
      {ok, Value};
    {error, Error, _Req2} ->
      %% TODO stop the pending processes; if we can...
      {error, Error};
    %% We've made a pass and there are no async pending calls
    Req2 = #req{pending = 0, changed = C} when C > 0 ->
      execute(Req2#req{changed = 0});
    %% There's nothing further to do so we wait
    Req2 = #req{changed = 0, pending = P} when P > 0 ->
      check_pending(Req2, 2000);
    %% There's some stuff that changed so we'll quickly check the mailbox
    Req2 = #req{changed = C, pending = P} when C > 0, P > 0 ->
      check_pending(Req2, 0);
    %% We're in a really bad state... We can't reduce any further and don't have the final value
    _Req2 ->
      {error, no_solution}
  end.

%% TODO handle any exits when we spawn workers
check_pending(Req = #req{ref = Ref, endname = End}, Timeout) ->
  receive
    {Status, Value, {Ref, Index}} when Index =:= End ->
      {Status, Value};
    {ok, Value, {Ref, Index}} ->
      Req2 = complete(Index, Req),
      Req3 = add_value(Index, Value, Req2),
      Pending = Req3#req.pending - 1,
      execute(Req3#req{pending = Pending});
    {error, Error, {Ref, _Index}} ->
      %% TODO stop the pending processes; if we can...
      {error, Error}
   after Timeout ->
     execute(Req)
   end.

digest(Req, []) ->
  Req;
digest(Req = #req{forms = ReqForms}, [{Index, _, _, _} = Form|Forms]) when Index band Req#req.required =:= 0 ->
  digest(Req#req{forms = [Form|ReqForms]}, Forms);
digest(Req = #req{changed = Changed, required = Required, forms = ReqForms},
             [{Index, {Cond, A, B}, '$cond', Deps}|Forms]) when Deps band Req#req.completed =:= Deps ->
  Dep = case fast_key:get(Cond, Req#req.values) of
    true -> A;
    _ -> B
  end,
  NewForm = {Index, Dep, '$alias', Dep},
  Req2 = Req#req{forms = [NewForm|ReqForms],
                 required = Required bor Dep,
                 changed = Changed + 1},
  digest(Req2, Forms);
digest(Req, [{Index, Dep, '$alias', Dep}|Forms]) when Dep band Req#req.completed =:= Dep ->
  Req2 = complete(Index, Req),
  Req3 = add_value(Index, fast_key:get(Dep, Req2#req.values), Req2),
  digest(Req3, Forms);
digest(Req, [{Index, {Mod, Fun, Args}, _Spawn, Deps}|Forms]) when Deps band Req#req.completed =:= Deps ->
  ResArgs = resolve_values(Args, [], Req#req.values),
  MapFun = Req#req.mapfun,
  Context = Req#req.context,
  %% TODO spawn_link request if true
  case MapFun(Mod, Fun, ResArgs, Context, self(), {Req#req.ref, Index}) of
    {ok, Value} when Index =:= Req#req.endname ->
      {ok, Value};
    {error, Error} ->
      {error, Error, Req};
    {ok, Value} ->
      Req2 = complete(Index, Req),
      Req3 = add_value(Index, Value, Req2),
      digest(Req3, Forms);
    pending ->
      Pending = Req#req.pending + 1,
      digest(Req#req{pending = Pending}, Forms)
  end;
digest(Req = #req{forms = ReqForms}, [Form|Forms]) ->
  digest(Req#req{forms = [Form|ReqForms]}, Forms).

%% TODO speedup?
resolve_values([], Args, _) ->
  lists:reverse(Args);
resolve_values([{'$exec', Index}|Rest], Args, Values) ->
  Value = fast_key:get(Index, Values),
  resolve_values(Rest, [Value|Args], Values);
resolve_values([Arg|Rest], Args, Values) when is_list(Arg) ->
  Arg2 = resolve_values(Arg, [], Values),
  resolve_values(Rest, [Arg2|Args], Values);
resolve_values([Arg|Rest], Args, Values) when is_tuple(Arg) ->
  List = tuple_to_list(Arg),
  Arg2 = resolve_values(List, [], Values),
  Arg3 = list_to_tuple(Arg2),
  resolve_values(Rest, [Arg3|Args], Values);
resolve_values([Arg|Rest], Args, Values) ->
  resolve_values(Rest, [Arg|Args], Values).

complete(I, Req = #req{completed = C, changed = Ch}) ->
  Req#req{completed = C bor I,
          changed = Ch + 1}.

add_value(Name, Value, Req = #req{values = Values}) ->
  Req#req{values = [{Name, Value}|Values]}.

%% utils

bitindex(I) ->
  trunc(math:pow(2, I)).
