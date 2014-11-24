-module(expr_runtime_pending).

-export([loop/1]).
-export([add/6]).

-compile(inline).
-compile({native, [o3]}).

-include("expr.hrl").

-define(IS_READY(Expr, State), Expr#expr.deps band State#state.completed =:= Expr#expr.deps).

%% if this clause executes you're in a bad place, big guy...
loop(State = #state{pending = [], iterations = I}) when I =:= 0 ->
  {error, no_solution, State};
loop(State = #state{pending = Pending}) ->
  loop(Pending, [], State).

loop([], Pending, State) ->
  {ok, State#state{pending = Pending}};

%%%
% literals.
%%%

%% the root is a literal so just return the value
loop([#expr{type = literal, value = Value, is_root = true}|_], _, State) ->
  {ok, Value, State};

%% set the literal value to the id in #state.values
loop([Expr = #expr{type = literal, value = Value}|Rest], Pending, State) ->
  State2 = expr_util:set_result(Value, Expr, State),
  loop(Rest, Pending, State2);

%%%
% compound types.
%%%

loop([Expr = #expr{type = list, status = waiting}|Rest], Pending, State) when ?IS_READY(Expr, State) ->
  {ok, Children} = resolve_values(Expr#expr.children, [], State#state.values),
  alias_value(Children, Expr, Rest, Pending, State);

loop([Expr = #expr{type = tuple, status = waiting}|Rest], Pending, State) when ?IS_READY(Expr, State) ->
  {ok, Children} = resolve_values(Expr#expr.children, [], State#state.values),
  alias_value(list_to_tuple(Children), Expr, Rest, Pending, State);

loop([Expr = #expr{type = map, status = waiting}|Rest], Pending, State) when ?IS_READY(Expr, State) ->
  {ok, Children} = resolve_values(Expr#expr.children, [], State#state.values),
  alias_value(maps:from_list(Children), Expr, Rest, Pending, State);

%%%
% calls.
%%%

%% the dependencies are ready so add the 'pending' function to the 'calls' list
loop([Expr = #expr{type = call, status = waiting}|Rest], Pending, State) when ?IS_READY(Expr, State) ->
  {ok, Children} = resolve_values(Expr#expr.children, [], State#state.values),
  Expr2 = Expr#expr{children = Children},
  loop(Rest, Pending, State#state{calls = [Expr2|State#state.calls]});

%%%
% conditions.
%%%

%% evaluate the first child in a condition
loop([Expr = #expr{type = 'cond', status = added, children = [Cond|_] = Children}|Rest], Pending, State) ->
  {Rest2, Pending2, State2} = add(Expr#expr{deps = 0, tmp = Children}, [], [Cond], Rest, Pending, State),
  loop(Rest2, Pending2, State2);

%% evaluate the selected branch of the condition
loop([Expr = #expr{type = 'cond', status = waiting}|Rest], Pending, State) when ?IS_READY(Expr, State) ->
  {ok, [Value]} = resolve_values(Expr#expr.children, [], State#state.values),
  Branch = case Value of
    true ->
      lists:nth(2, Expr#expr.tmp);
    false ->
      lists:nth(3, Expr#expr.tmp);
    Arg ->
      {error, {cond_badarg, Arg}, State}
  end,
  case Branch of
    {error, _, _} = Error ->
      Error;
    Branch ->
      {Rest2, [Expr2|Pending2], State2} = add(Expr#expr{deps = 0}, [], [Branch], Rest, Pending, State),
      Expr3 = Expr2#expr{status = branching},
      loop(Rest2, [Expr3|Pending2], State2)
  end;

%% set the result of the branch
loop([Expr = #expr{type = 'cond', status = branching}|Rest], Pending, State) when ?IS_READY(Expr, State) ->
  {ok, [Value]} = resolve_values(Expr#expr.children, [], State#state.values),
  alias_value(Value, Expr, Rest, Pending, State);

%%%
% comprehensions.
%%%

%% evaluate the input list in the comprehension
loop([Expr = #expr{type = comprehension, status = added, children = [Input|_] = Children}|Rest], Pending, State) ->
  {Rest2, Pending2, State2} = add(Expr#expr{deps = 0, tmp = Children}, [], [Input], Rest, Pending, State),
  loop(Rest2, Pending2, State2);

%% for each value in the list push the expression on 'pending'
loop([Expr = #expr{type = comprehension, tmp = [_, Var, ChildExpr], status = waiting}|Rest],
      Pending, State) when ?IS_READY(Expr, State) ->
  %% TODO replace with  {ok, [List]} = resolve_values(Expr#expr.children, [], State#state.values),
  List = maps:get(Expr#expr.deps, State#state.values),
  {ok, Children, State2} = add_comprehension(List, Var, ChildExpr, State, []),
  {Rest2, [Expr2|Pending2], State3} = add(Expr#expr{deps = 0}, [], Children, Rest, Pending, State2),
  Expr3 = Expr2#expr{status = iterating},
  loop(Rest2, [Expr3|Pending2], State3);

%% set the result of the comprehension
loop([Expr = #expr{type = comprehension, status = iterating}|Rest], Pending, State) when ?IS_READY(Expr, State) ->
  {ok, Values} = resolve_values(Expr#expr.children, [], State#state.values),
  alias_value(Values, Expr, Rest, Pending, State);

%%%
% recently added.
%%%

%% the dependencies have not been added to the pending list
loop([Expr = #expr{children = Children, status = added, deps = -1}|Rest], Pending, State)  ->
  {Rest2, Pending2, State2} = add(Expr#expr{deps = 0}, [], Children, Rest, Pending, State),
  loop(Rest2, Pending2, State2);
loop([Expr = #expr{children = Children, status = added}|Rest], Pending, State)  ->
  {Rest2, Pending2, State2} = add(Expr, [], Children, Rest, Pending, State),
  loop(Rest2, Pending2, State2);

%% continue on... REMOVE THIS ONCE ALL IS READY!!! - there should be a match for all previous patterns or crash
loop([Expr|Rest], Pending, State) ->
  loop(Rest, [Expr|Pending], State).

%%% add the children to the 'pending' list

add(Expr, NewChildren, [], Rest, Pending, State) ->
  {Rest, [Expr#expr{status = waiting, children = lists:reverse(NewChildren)}|Pending], State};

%% pull the child from the vars map if integer
add(Expr = #expr{deps = Deps}, NewChildren, [Child|Children], Rest, Pending,
    State = #state{vars = Vars, waiting = Waiting}) when is_integer(Child) ->
  ChildExpr = maps:get(Child, Vars),
  Expr2 = Expr#expr{deps = Deps bor Child},
  Rest2 = case Child band Waiting =:= 0 of
    true ->
      [ChildExpr|Rest];
    _ ->
      Rest
  end,
  add(Expr2, [Child|NewChildren], Children, Rest2, Pending, State#state{waiting = Waiting bor Child});

%% add a child call expression
add(Expr = #expr{deps = Deps}, NewChildren, [Child = #expr{type = Type}|Children], Rest, Pending, State) when
    Type =:= call orelse Type =:= list orelse Type =:= tuple orelse Type =:= map orelse Type =:= 'cond' orelse Type =:= comprehension ->
  {ID, State2} = expr_util:next_id(State),
  Expr2 = Expr#expr{deps = Deps bor ID},
  Child2 = Child#expr{id = ID},
  Rest2 = [Child2|Rest],
  Waiting = State#state.waiting,
  add(Expr2, [ID|NewChildren], Children, Rest2, Pending, State2#state{waiting = Waiting bor ID});

%% pass on the literals
add(Expr, NewChildren, [Child = #expr{type = literal}|Children], Rest, Pending, State) ->
  add(Expr, [Child|NewChildren], Children, Rest, Pending, State).

%% resolve all of the needed arguments

resolve_values([], Acc, _) ->
  {ok, lists:reverse(Acc)};
resolve_values([Child|Children], Acc, Values) when is_integer(Child) ->
  Value = maps:get(Child, Values),
  resolve_values(Children, [Value|Acc], Values);
resolve_values([#expr{type = literal, value = Value}|Children], Acc, Values) ->
  resolve_values(Children, [Value|Acc], Values).

alias_value(Value, Expr, _, _, State) when Expr#expr.is_root ->
  {ok, Value, State};
alias_value(Value, Expr, Rest, Pending, State) ->
  State2 = expr_util:set_result(Value, Expr, State),
  loop(Rest, Pending, State2).

add_comprehension([], _, _, State, Acc) ->
  {ok, lists:reverse(Acc), State};
add_comprehension([Value|Rest], Var, Expr, State, Acc) ->
  Literal = #expr{type = literal, value = Value},
  {ok, [Expr2]} = expr_util:replace_variable(Var, Literal, [Expr], []),
  add_comprehension(Rest, Var, Expr, State, [Expr2|Acc]).
