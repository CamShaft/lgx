-module(expr_compiler).

-export([compile/1]).

-include("expr.hrl").

-define(EXPR_KEY, '__EXPR_ID').
-define(ROOT_KEY, '__EXPR_ROOT').
-define(VAR_KEY, '__EXPR_VAR').

-define(PASSES, [
  fun mark_root/2,
  fun flatten/2,
  fun resolve_variables/2,

  %% fun set_conds/2
  fun raise_ids/2,
  fun compute_deps/2,
  fun to_records/2
]).

compile(Exprs) ->
  compile(Exprs, ?PASSES).

compile(Exprs, []) ->
  {ok, Exprs};
compile(Exprs, [Pass|Passes]) ->
  case Pass(Exprs, []) of
    {ok, Exprs2} ->
      compile(Exprs2, Passes);
    Error ->
      Error
  end.

%% Mark the last expression as the root expression so we know where to start

mark_root(Exprs, _) ->
  [Expr|Rest] = lists:reverse(Exprs),
  {ok, lists:reverse([Expr#{?ROOT_KEY => true}|Rest])}.

%% Flatten the tree into a list

flatten([], Acc) ->
  {ok, lists:reverse(Acc)};

flatten([#{type := literal} = Expr|Exprs], Acc) ->
  Expr2 = Expr#{?EXPR_KEY => fid(Acc)},
  flatten(Exprs, [Expr2|Acc]);

flatten([#{type := variable} = Expr|Exprs], Acc) ->
  Expr2 = Expr#{?EXPR_KEY => fid(Acc)},
  flatten(Exprs, [Expr2|Acc]);

flatten([#{type := assign, value := Value, children := Children}|Exprs], Acc) ->
  [Key] = maps:keys(Children),
  Expr = maps:get(Key, Children),
  {ok, [Expr2|Acc2]} = flatten([Expr], Acc),

  Expr3 = case maps:is_key(?ROOT_KEY, Expr2) of
    true ->
      Expr2#{?ROOT_KEY => true};
    _ ->
      Expr2
  end,

  flatten(Exprs, [Expr3#{?VAR_KEY => Value}|Acc2]);

flatten([#{children := Children} = Expr|Exprs], Acc) ->
  {Children2, Acc2} = flatten_children(maps:keys(Children), Children, Acc),
  Expr2 = Expr#{?EXPR_KEY => fid(Acc2), children := Children2},
  flatten(Exprs, [Expr2|Acc2]).


flatten_children([], Children, Acc) ->
  {Children, Acc};
flatten_children([Key|Keys], Children, Acc) ->
  {ok, Acc2} = flatten([maps:get(Key, Children)], Acc),
  Children2 = maps:update(Key, fid(Acc2) - 1, Children),
  flatten_children(Keys, Children2, Acc2).

fid(Acc) ->
  length(Acc).

%% Resolve any variable names

resolve_variables(Exprs, _) ->
  find_assignments(Exprs, Exprs).

find_assignments([], Exprs) ->
  {ok, Exprs};
find_assignments([#{?VAR_KEY := Var, ?EXPR_KEY := ID}|Rest], Exprs) ->
  {ok, Exprs2} = replace_variables(Var, ID, Exprs, []),
  find_assignments(Rest, Exprs2);
find_assignments([_|Rest], Exprs) ->
  find_assignments(Rest, Exprs).

replace_variables(_, _, [], Acc) ->
  {ok, lists:reverse(Acc)};
replace_variables(Var, ID, [#{type := variable, value := Var} = Expr|Exprs], Acc) ->
  Expr2 = Expr#{children => #{0 => ID}},
  replace_variables(Var, ID, Exprs, [Expr2|Acc]);
replace_variables(Var, ID, [Expr|Exprs], Acc) ->
  replace_variables(Var, ID, Exprs, [Expr|Acc]).

%% TODO find undefined variables
%% TODO remove any unused values
%% TODO replace any single use variables
%% TODO turn any lists, tuples, or maps into literals if no computation is needed
%% TODO add default line number

%% Mark the conditional expressions optional

%% set_conds([], Acc) ->
%%   {ok, lists:reverse(Acc)};
%% set_conds([#{type := 'cond', children := Children} = Expr|Exprs], Acc) ->
%%   set_conds(Exprs, [Expr2|Acc]);
%% set_conds([Expr|Exprs], Acc) ->
%%   set_conds(Exprs, [Expr|Acc]);

%% Raise IDs 2^n

raise_ids([], Acc) ->
  {ok, lists:reverse(Acc)};
raise_ids([#{children := Children, ?EXPR_KEY := ID} = Expr|Exprs], Acc) ->
  Children2 = maps:map(fun(_, CID) -> raise_id(CID) end, Children),
  raise_ids(Exprs, [Expr#{children := Children2, ?EXPR_KEY := raise_id(ID)}|Acc]);
raise_ids([#{?EXPR_KEY := ID} = Expr|Exprs], Acc) ->
  raise_ids(Exprs, [Expr#{?EXPR_KEY := raise_id(ID)}|Acc]);
raise_ids([Expr|Exprs], Acc) ->
  raise_ids(Exprs, [Expr|Acc]).

raise_id(ID) ->
  trunc(math:pow(2, ID)).

%% Compute the expressions dependencies

compute_deps([], Acc) ->
  {ok, lists:reverse(Acc)};
compute_deps([#{children := Children} = Expr|Exprs], Acc) ->
  ComputedDeps = lists:foldl(fun compute_dep/2, 0, maps:values(Children)),
  compute_deps(Exprs, [Expr#{deps => ComputedDeps}|Acc]);
compute_deps([Expr|Exprs], Acc) ->
  compute_deps(Exprs, [Expr|Acc]).

compute_dep(Dep, Deps) ->
  Deps bor Dep.

%% Convert the expressions to records for fast runtime lookup

to_records([], Acc) ->
  {ok, lists:reverse(Acc)};
to_records([Expr|Exprs], Acc) ->
  {ok, Req} = to_record(Expr),
  %% TODO set is_root if has ?EXPR_ROOT
  to_records(Exprs, [Req|Acc]).

to_record(#{?EXPR_KEY := ID, type := literal, value := Value, line := Line}) ->
  {ok, #expr{
    id = ID,
    value = Value,
    line = Line
  }};
to_record(#{?EXPR_KEY := ID, type := list, children := Children, deps := Deps, line := Line}) ->
  Value = maps:values(Children),
  {ok, #expr{
    id = ID,
    type = list,
    value = Value,
    deps = Deps,
    line = Line
  }};
to_record(#{?EXPR_KEY := ID, type := tuple, children := Children, deps := Deps, line := Line}) ->
  Value = maps:values(Children), %% call list_to_tuple later since the map should be pretty fast
  {ok, #expr{
    id = ID,
    type = tuple,
    value = Value,
    deps = Deps,
    line = Line
  }};
to_record(#{?EXPR_KEY := ID, type := map, children := Children, deps := Deps, line := Line}) ->
  {ok, #expr{
    id = ID,
    type = map,
    value = Children,
    deps = Deps,
    line = Line
  }};
to_record(Expr) ->
  {ok, Expr}.
