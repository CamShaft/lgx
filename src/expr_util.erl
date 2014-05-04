-module(expr_util).

-include("expr.hrl").

-export([set_result/3]).
-export([next_id/1]).
-export([replace_variable/4]).

%% TODO clear the pid
%% set the value for the id
set_result(Value, #expr{id = ID}, State) ->
  set_result(Value, ID, State);
set_result(Value, ID, State = #state{values = Values, completed = Completed, waiting = Waiting}) when is_integer(ID) ->
  Values2 = maps:put(ID, Value, Values),
  State#state{values = Values2, completed = Completed bor ID, waiting = Waiting bxor ID}.

%% return an id (2^n)
next_id(State = #state{counter = Counter}) ->
  {trunc(math:pow(2, Counter)), State#state{counter = Counter + 1}}.

replace_variable(_, _, [], Acc) ->
  {ok, lists:reverse(Acc)};
replace_variable(Var, Value, [#expr{type = variable, value = Var}|Rest], Acc) ->
  replace_variable(Var, Value, Rest, [Value|Acc]);
replace_variable(Var, Value, [Expr = #expr{children = Children}|Rest], Acc) when is_list(Children) ->
  {ok, Children2} = replace_variable(Var, Value, Children, []),
  replace_variable(Var, Value, Rest, [Expr#expr{children = Children2}|Acc]);
replace_variable(Var, Value, [Expr|Rest], Acc) ->
  replace_variable(Var, Value, Rest, [Expr|Acc]).
