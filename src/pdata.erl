-module(pdata).

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
  changed = 0
}).

compile(Defs) ->
  [Def|_] = Defs,
  Name = erlang:element(1, Def),
  compile(Defs, Name).
compile(Defs, Start) ->
  Normalized = normalize_defs(Defs, []),
  {ok, {lists:reverse(connect_deps(Normalized, Start)), Start}}.

execute({Forms, End}, MapFun, Context) ->
  execute(#req{ref = erlang:make_ref(),
               forms = Forms,
               mapfun = MapFun,
               context = Context,
               endname = End}).

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

normalize_def({Name, Mod, Fun, Args}, Index) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
  {Name, Index, {Mod, Fun, Args}, false, undefined, undefined};
normalize_def({Name, Mod, Fun, Args, spawn}, Index) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
  {Name, Index, {Mod, Fun, Args}, spawn, undefined, undefined};
normalize_def(Fun, _) ->
  erlang:error({invalid_fun, Fun}).

%% TODO detect circular dependencies
connect_deps(Defs, Start) ->
  case lists:keyfind(Start, 1, Defs) of
    false ->
      erlang:error({not_found, Start});
    {Name, Index, MFA, Spawn, _, _} = StartFun ->
      Deps = scan_deps(StartFun),
      DepIndicies = [dep_index(Dep, Defs) || {Dep, _Path} <- Deps],
      EncDeps = encode_deps(DepIndicies, 0),
      Defs2 = lists:keystore(Start, 1, Defs, {Name, Index, MFA, Spawn, Deps, EncDeps}),
      resolve_deps(Start, Deps, Defs2)
  end.

dep_index(Dep, Defs) ->
  case lists:keyfind(Dep, 1, Defs) of
    false ->
      erlang:error({not_found, Dep});
    {_, Index, _, _, _, _} ->
      Index
  end.

encode_deps([], Num) ->
  Num;
encode_deps([I|Indicies], Num) ->
  Num2 = bitindex(I) bor Num,
  encode_deps(Indicies, Num2).

resolve_deps(_, [], Defs) ->
  Defs;
resolve_deps(Start, [{Dep, _}|Deps], Defs) ->
  Defs2 = connect_deps(Defs, Dep),
  resolve_deps(Start, Deps, Defs2).

scan_deps({_, _, {_, _, Args}, _, _, _}) ->
  scan_args(Args, [], [], 0).

scan_args([], Deps, _, _) ->
  lists:reverse(Deps);
scan_args([{'$exec', Fun}|Args], Deps, Path, Index) ->
  scan_args(Args, [{Fun, Path ++ [Index]}|Deps], Path, Index + 1);
scan_args([Arg|Args], Deps, Path, Index) when is_list(Arg) ->
  Scanned = scan_args(Arg, Deps, Path ++ [Index], 0),
  scan_args(Args, Scanned, Path, Index);
scan_args([Arg|Args], Deps, Path, Index) when is_tuple(Arg) ->
  TList = [element(I, Arg) || I <- lists:seq(1, tuple_size(Arg))],
  Scanned = scan_args(TList, Deps, Path ++ [Index], 0),
  scan_args(Args, Scanned, Path, Index);
scan_args([_|Args], Deps, Path, Index) ->
  scan_args(Args, Deps, Path, Index + 1).

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
    _ ->
      {error, no_solution}
  end.

%% TODO handle any exits when we spawn workers
check_pending(Req = #req{ref = Ref, endname = End}, Timeout) ->
  receive
    {Status, Value, {Ref, _, Name}} when Name =:= End ->
      {Status, Value};
    {ok, Value, {Ref, Index, Name}} ->
      Req2 = complete(Index, Req),
      Req3 = add_value(Name, Value, Req2),
      Pending = Req3#req.pending - 1,
      execute(Req3#req{pending = Pending});
    {error, Error, {Ref, _Index, _Name}} ->
      %% TODO stop the pending processes; if we can...
      {error, Error}
   after Timeout ->
     execute(Req)
   end.

digest(Req, []) ->
  Req;
digest(Req, [{Name, Index, {Mod, Fun, Args}, _Spawn, Deps, Enc}|Forms])
    when Enc band Req#req.completed =:= Enc ->
  ResArgs = resolve_values(Args, Deps, Req#req.values),
  MapFun = Req#req.mapfun,
  Context = Req#req.context,
  %% TODO spawn_link request if true
  case MapFun(Mod, Fun, ResArgs, Context, self(), {Req#req.ref, Index, Name}) of
    {ok, Value} when Name =:= Req#req.endname ->
      {ok, Value};
    {error, Error} ->
      {error, Error, Req};
    {ok, Value} ->
      Req2 = complete(Index, Req),
      Req3 = add_value(Name, Value, Req2),
      digest(Req3, Forms);
    pending ->
      Pending = Req#req.pending + 1,
      digest(Req#req{pending = Pending}, Forms)
  end;
digest(Req = #req{forms = ReqForms}, [Form|Forms]) ->
  digest(Req#req{forms = [Form|ReqForms]}, Forms).

resolve_values(Args, [], _) ->
  Args;
resolve_values(Arguments, [{Dep, Path}|Deps], Values) ->
  Value = fast_key:get(Dep, Values),
  ResolvedArguments = set_at_path(Path, Value, Arguments),
  resolve_values(ResolvedArguments, Deps, Values).

%% TODO speed this up
set_at_path([], Value, _) ->
  Value;
set_at_path([I|Path], Value, Arguments) when is_list(Arguments) ->
  {Head, [Target|Tail]} = lists:split(I, Arguments),
  Head ++ [set_at_path(Path, Value, Target)] ++ Tail;
set_at_path([I|Path], Value, Arguments) when is_tuple(Arguments) ->
  Element = erlang:element(I + 1, Arguments),
  erlang:setelement(I + 1, Arguments, set_at_path(Path, Value, Element)).

complete(I, Req = #req{completed = C, changed = Ch}) ->
  Req#req{completed = C bor bitindex(I),
          changed = Ch + 1}.

add_value(Name, Value, Req = #req{values = Values}) ->
  Req#req{values = [{Name, Value}|Values]}.

%% utils

bitindex(I) ->
  trunc(math:pow(2, I)).
