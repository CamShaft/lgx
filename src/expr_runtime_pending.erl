-module(expr_runtime_pending).

-export([loop/1]).
-export([add/8]).

-compile(inline).
-compile({native, [o3]}).

-include("expr.hrl").

-define(IS_READY(Expr, Completed), Expr#expr.deps band Completed =:= Expr#expr.deps).

%% if this clause executes you're in a bad place, big guy...
loop(State = #state{pending = [], iterations = I}) when I =:= 0 ->
  {error, no_solution, State};

loop(State = #state{pending = Pending, values = Values, completed = Completed, waiting = Waiting, vars = Vars, counter = Counter}) ->
  case loop(Pending, [], Values, [], Completed, Waiting, Counter, Vars) of
    {ok, Pending2, Values2, Calls2, Completed2, Waiting2, Counter2} ->
      {ok, State#state{pending = Pending2, values = Values2, calls = Calls2, completed = Completed2, waiting = Waiting2, counter = Counter2}};
    {ok, Value} ->
      {ok, Value, State};
    {error, Error} ->
      {error, Error, State}
  end.

loop([], Pending, Values, Calls, Completed, Waiting, Counter, _Vars) ->
  {ok, Pending, Values, Calls, Completed, Waiting, Counter};

%%%
% literals.
%%%

%% the root is a literal so just return the value
loop([#expr{type = literal, value = Value, is_root = true}|_], _Pending, _Values, _Calls, _Completed, _Waiting, _Counter, _Vars) ->
  {ok, Value};

%% set the literal value to the id in #state.values
loop([Expr = #expr{type = literal, value = Value}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars) ->
  {Values2, Completed2, Waiting2} = expr_util:set_result(Value, Expr, Values, Completed, Waiting),
  loop(Rest, Pending, Values2, Calls, Completed2, Waiting2, Counter, Vars);

%%%
% compound types.
%%%

loop([Expr = #expr{type = list, status = waiting}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars) when ?IS_READY(Expr, Completed) ->
  {ok, Children} = resolve_values(Expr#expr.children, [], Values),
  alias_value(Children, Expr, Rest, Pending, Values, Calls, Completed, Waiting, Counter, Vars);

loop([Expr = #expr{type = tuple, status = waiting}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars) when ?IS_READY(Expr, Completed) ->
  {ok, Children} = resolve_values(Expr#expr.children, [], Values),
  alias_value(list_to_tuple(Children), Expr, Rest, Pending, Values, Calls, Completed, Waiting, Counter, Vars);

loop([Expr = #expr{type = map, status = waiting}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars) when ?IS_READY(Expr, Completed) ->
  {ok, Children} = resolve_values(Expr#expr.children, [], Values),
  alias_value(maps:from_list(Children), Expr, Rest, Pending, Values, Calls, Completed, Waiting, Counter, Vars);

%%%
% calls.
%%%

%% the dependencies are ready so add the 'pending' function to the 'calls' list
loop([Expr = #expr{type = call, status = waiting}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars) when ?IS_READY(Expr, Completed) ->
  {ok, Children} = resolve_values(Expr#expr.children, [], Values),
  Expr2 = Expr#expr{children = Children},
  loop(Rest, Pending, Values, [Expr2|Calls], Completed, Waiting, Counter, Vars);

%%%
% conditions.
%%%

%% evaluate the first child in a condition
loop([Expr = #expr{type = 'cond', status = added, children = [Cond|_] = Children}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars) ->
  {Rest2, Pending2, Waiting2, Counter2} = add(Expr#expr{deps = 0, tmp = Children}, [], [Cond], Rest, Pending, Waiting, Counter, Vars),
  loop(Rest2, Pending2, Values, Calls, Completed, Waiting2, Counter2, Vars);

%% evaluate the selected branch of the condition
loop([Expr = #expr{type = 'cond', status = waiting}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars) when ?IS_READY(Expr, Completed) ->
  {ok, [Value]} = resolve_values(Expr#expr.children, [], Values),
  Branch = case Value of
    true ->
      lists:nth(2, Expr#expr.tmp);
    false ->
      lists:nth(3, Expr#expr.tmp);
    Arg ->
      {error, {cond_badarg, Arg}}
  end,
  case Branch of
    {error, _} = Error ->
      Error;
    Branch ->
      {Rest2, [Expr2|Pending2], Waiting2, Counter2} = add(Expr#expr{deps = 0}, [], [Branch], Rest, Pending, Waiting, Counter, Vars),
      Expr3 = Expr2#expr{status = branching},
      loop(Rest2, [Expr3|Pending2], Values, Calls, Completed, Waiting2, Counter2, Vars)
  end;

%% set the result of the branch
loop([Expr = #expr{type = 'cond', status = branching}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars) when ?IS_READY(Expr, Completed) ->
  {ok, [Value]} = resolve_values(Expr#expr.children, [], Values),
  alias_value(Value, Expr, Rest, Pending, Values, Calls, Completed, Waiting, Counter, Vars);

%%%
% comprehensions.
%%%

%% evaluate the input list in the comprehension
loop([Expr = #expr{type = comprehension, status = added, children = [Input|_] = Children}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars) ->
  {Rest2, Pending2, Waiting2, Counter2} = add(Expr#expr{deps = 0, tmp = Children}, [], [Input], Rest, Pending, Waiting, Counter, Vars),
  loop(Rest2, Pending2, Values, Calls, Completed, Waiting2, Counter2, Vars);

%% for each value in the list push the expression on 'pending'
loop([Expr = #expr{type = comprehension, tmp = [_, Var, ChildExpr], status = waiting}|Rest],
      Pending, Values, Calls, Completed, Waiting, Counter, Vars) when ?IS_READY(Expr, Completed) ->
  %% TODO replace with  {ok, [List]} = resolve_values(Expr#expr.children, [], State#state.values),
  List = maps:get(Expr#expr.deps, Values),
  {ok, Children} = add_comprehension(List, Var, ChildExpr, []),
  {Rest2, [Expr2|Pending2], Waiting2, Counter2} = add(Expr#expr{deps = 0}, [], Children, Rest, Pending, Waiting, Counter, Vars),
  Expr3 = Expr2#expr{status = iterating},
  loop(Rest2, [Expr3|Pending2], Values, Calls, Completed, Waiting2, Counter2, Vars);

%% set the result of the comprehension
loop([Expr = #expr{type = comprehension, status = iterating}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars) when ?IS_READY(Expr, Completed) ->
  {ok, Children} = resolve_values(Expr#expr.children, [], Values),
  alias_value(Children, Expr, Rest, Pending, Values, Calls, Completed, Waiting, Counter, Vars);

%%%
% recently added.
%%%

%% the dependencies have not been added to the pending list
loop([Expr = #expr{children = Children, status = added, deps = -1}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars)  ->
  {Rest2, Pending2, Waiting2, Counter2} = add(Expr#expr{deps = 0}, [], Children, Rest, Pending, Waiting, Counter, Vars),
  loop(Rest2, Pending2, Values, Calls, Completed, Waiting2, Counter2, Vars);
loop([Expr = #expr{children = Children, status = added}|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars)  ->
  {Rest2, Pending2, Waiting2, Counter2} = add(Expr, [], Children, Rest, Pending, Waiting, Counter, Vars),
  loop(Rest2, Pending2, Values, Calls, Completed, Waiting2, Counter2, Vars);

%% continue on... REMOVE THIS ONCE ALL IS READY!!! - there should be a match for all previous patterns or crash
loop([Expr|Rest], Pending, Values, Calls, Completed, Waiting, Counter, Vars) ->
  loop(Rest, [Expr|Pending], Values, Calls, Completed, Waiting, Counter, Vars).

%%% add the children to the 'pending' list

add(Expr, NewChildren, [], Rest, Pending, Waiting, Counter, _Vars) ->
  {Rest, [Expr#expr{status = waiting, children = lists:reverse(NewChildren)}|Pending], Waiting, Counter};

%% pull the child from the vars map if integer
add(Expr = #expr{deps = Deps}, NewChildren, [Child|Children], Rest, Pending, Waiting, Counter, Vars) when is_integer(Child) ->
  ChildExpr = maps:get(Child, Vars),
  Expr2 = Expr#expr{deps = Deps bor Child},
  Rest2 = case Child band Waiting =:= 0 of
    true ->
      [ChildExpr|Rest];
    _ ->
      Rest
  end,
  add(Expr2, [Child|NewChildren], Children, Rest2, Pending, Waiting bor Child, Counter, Vars);

%% add a child call expression
add(Expr = #expr{deps = Deps}, NewChildren, [Child = #expr{type = Type}|Children], Rest, Pending, Waiting, Counter, Vars) when
    Type =:= call orelse Type =:= list orelse Type =:= tuple orelse Type =:= map orelse Type =:= 'cond' orelse Type =:= comprehension ->
  {ID, Counter2} = expr_util:next_id(Counter),
  Expr2 = Expr#expr{deps = Deps bor ID},
  Child2 = Child#expr{id = ID},
  Rest2 = [Child2|Rest],
  add(Expr2, [ID|NewChildren], Children, Rest2, Pending, Waiting bor ID, Counter2, Vars);

%% pass on the literals
add(Expr, NewChildren, [Child = #expr{type = literal}|Children], Rest, Pending, Waiting, Counter, Vars) ->
  add(Expr, [Child|NewChildren], Children, Rest, Pending, Waiting, Counter, Vars).

%% resolve all of the needed arguments

resolve_values([], Acc, _) ->
  {ok, lists:reverse(Acc)};
resolve_values([Child|Children], Acc, Values) when is_integer(Child) ->
  Value = maps:get(Child, Values),
  resolve_values(Children, [Value|Acc], Values);
resolve_values([#expr{type = literal, value = Value}|Children], Acc, Values) ->
  resolve_values(Children, [Value|Acc], Values).

alias_value(Value, Expr, _, _Pending, _Values, _Calls, _Completed, _Waiting, _Counter, _Vars) when Expr#expr.is_root ->
  {ok, Value};
alias_value(Value, Expr, Rest, Pending, Values, Calls, Completed, Waiting, Counter, Vars) ->
  {Values2, Completed2, Waiting2} = expr_util:set_result(Value, Expr, Values, Completed, Waiting),
  loop(Rest, Pending, Values2, Calls, Completed2, Waiting2, Counter, Vars).

add_comprehension([], _, _, Acc) ->
  {ok, lists:reverse(Acc)};
add_comprehension([Value|Rest], Var, Expr, Acc) ->
  Literal = #expr{type = literal, value = Value},
  {ok, [Expr2]} = expr_util:replace_variable(Var, Literal, [Expr], []),
  add_comprehension(Rest, Var, Expr, [Expr2|Acc]).
