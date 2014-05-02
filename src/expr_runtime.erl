-module(expr_runtime).

-export([execute/3]).

-include("expr.hrl").

-define(IS_READY(Expr, State), Expr#expr.deps band State#state.completed =:= Expr#expr.deps).

execute([], _, _) ->
  {ok, undefined};
execute(State, MapFun, Context) ->
  loop(State#state{map = MapFun, context = Context, ref = make_ref()}).

%%%%%%%
%% main loop.
%%%%%%%

loop(State = #state{stalled = 100}) ->
  {error, infinite_loop, State};
loop(State) ->
  case pending_loop(State) of
    %% we're done
    {ok, Value, PendingState} ->
      {ok, Value, PendingState};
    {error, Error, PendingState} ->
      handle_error(Error, PendingState);
    {ok, PendingState} ->
      case receive_loop(PendingState) of
        {error, Error, ReceiveState} ->
          handle_error(Error, ReceiveState);
        {ok, ReceiveState} ->
          case exec_loop(ReceiveState) of
            {error, Error, ExecState} ->
               handle_error(Error, ExecState);
            %% we're done!
            {ok, Value, ExecState} ->
              {ok, Value, ExecState};
            {ok, State = #state{iterations = Iter, stalled = Stalled}} ->
               loop(State#state{iterations = Iter + 1, stalled = Stalled + 1});
            {ok, ExecState = #state{iterations = Iter}} ->
               loop(ExecState#state{iterations = Iter + 1, stalled = 0})
          end
      end
  end.

%%%%%%%
%% pending loop.
%%%%%%%

%% if this clause executes you're in a bad place, big guy...
pending_loop(State = #state{pending = [], iterations = I}) when I =:= 0 ->
  {error, no_solution, State};
pending_loop(State = #state{pending = Pending}) ->
  pending_loop(Pending, [], State).

pending_loop([], Pending, State) ->
  {ok, State#state{pending = Pending}};

%%%
% literals.
%%%

%% the root is a literal so just return the value
pending_loop([#expr{type = literal, value = Value, is_root = true}|_], _, State) ->
  {ok, Value, State};

%% set the literal value to the id in #state.values
pending_loop([Expr = #expr{type = literal, value = Value}|Rest], Pending, State) ->
  State2 = set_result(Value, Expr, State),
  pending_loop(Rest, Pending, State2);

%%%
% calls.
%%%

%% the dependencies are ready so add the 'pending' function to the 'calls' list
pending_loop([Expr = #expr{type = call,status = waiting}|Rest],
              Pending,
              State = #state{calls = Calls, values = Values}) when ?IS_READY(Expr, State) ->
  {ok, Children} = resolve_values(Expr#expr.children, [], Values),
  Expr2 = Expr#expr{children = Children},
  pending_loop(Rest, Pending, State#state{calls = [Expr2|Calls]});

%%%
% conditions.
%%%

%% evaluate the first child in a condition
pending_loop([Expr = #expr{type = 'cond', status = added, children = [Cond|_] = Children}|Rest], Pending, State) ->
  {Rest2, Pending2, State2} = pending_add(Expr#expr{deps = 0, tmp = Children}, [], [Cond], Rest, Pending, State),
  pending_loop(Rest2, Pending2, State2);

%% evaluate the selected branch of the condition
pending_loop([Expr = #expr{type = 'cond', status = waiting}|Rest], Pending, State) when ?IS_READY(Expr, State) ->
  Children = Expr#expr.tmp,
  Branch = case maps:get(Expr#expr.deps, State#state.values) of
    true ->
      lists:nth(2, Children);
    false ->
      lists:nth(3, Children);
    Arg ->
      {error, {cond_badarg, Arg}, State}
  end,
  case Branch of
    {error, _, _} = Error ->
      Error;
    Branch ->
      {Rest2, [Expr2|Pending2], State2} = pending_add(Expr#expr{deps = 0}, [], [Branch], Rest, Pending, State),
      Expr3 = Expr2#expr{status = branching},
      pending_loop(Rest2, [Expr3|Pending2], State2)
  end;

%% set the result of the branch
pending_loop([Expr = #expr{type = 'cond', status = branching}|Rest], Pending, State) when ?IS_READY(Expr, State) ->
  Value = maps:get(Expr#expr.deps, State#state.values),
  pending_alias_value(Value, Expr, Rest, Pending, State);

%%%
% comprehensions.
%%%

%% evaluate the input list in the comprehension
pending_loop([Expr = #expr{type = comprehension, status = added, children = [Input|_] = Children}|Rest], Pending, State) ->
  {Rest2, Pending2, State2} = pending_add(Expr#expr{deps = 0, tmp = Children}, [], [Input], Rest, Pending, State),
  pending_loop(Rest2, Pending2, State2);

%% for each value in the list push the expression on 'pending'
pending_loop([Expr = #expr{type = comprehension, tmp = [_, Var, ChildExpr], status = waiting}|Rest],
              Pending,
              State) when ?IS_READY(Expr, State) ->
  List = maps:get(Expr#expr.deps, State#state.values),
  {ok, Children, State2} = pending_add_comprehension(List, Var, ChildExpr, State, []),
  {Rest2, [Expr2|Pending2], State3} = pending_add(Expr#expr{deps = 0}, [], Children, Rest, Pending, State2),
  Expr3 = Expr2#expr{status = iterating},
  pending_loop(Rest2, [Expr3|Pending2], State3);

%% set the result of the comprehension
pending_loop([Expr = #expr{type = comprehension, status = iterating}|Rest], Pending, State) when ?IS_READY(Expr, State) ->
  {ok, Values} = resolve_values(Expr#expr.children, [], State#state.values),
  pending_alias_value(Values, Expr, Rest, Pending, State);

pending_loop([Expr = #expr{type = comprehension, status = iterating}|Rest], Pending, State) ->
  pending_loop(Rest, [Expr|Pending], State);

%%%
% recently added.
%%%

%% the dependencies have not been added to the pending list
pending_loop([Expr = #expr{children = Children, status = added, deps = -1}|Rest], Pending, State)  ->
  {Rest2, Pending2, State2} = pending_add(Expr#expr{deps = 0}, [], Children, Rest, Pending, State),
  pending_loop(Rest2, Pending2, State2);
pending_loop([Expr = #expr{children = Children, status = added}|Rest], Pending, State)  ->
  {Rest2, Pending2, State2} = pending_add(Expr, [], Children, Rest, Pending, State),
  pending_loop(Rest2, Pending2, State2);

%% continue on... REMOVE THIS ONCE ALL IS READY!!! - there should be a match for all previous patterns or crash
pending_loop([Expr|Rest], Pending, State) ->
  pending_loop(Rest, [Expr|Pending], State).

%%% add the children to the 'pending' list

pending_add(Expr, NewChildren, [], Rest, Pending, State) ->
  {Rest, [Expr#expr{status = waiting, children = lists:reverse(NewChildren)}|Pending], State};

%% pull the child from the vars map if integer
pending_add(Expr = #expr{deps = Deps}, NewChildren, [Child|Children], Rest, Pending,
            State = #state{vars = Vars, waiting = Waiting}) when is_integer(Child)  ->
  ChildExpr = maps:get(Child, Vars),
  Expr2 = Expr#expr{deps = Deps bor Child},
  Rest2 = case Child band Waiting =:= 0 of
    true ->
      [ChildExpr|Rest];
    _ ->
      Rest
  end,
  pending_add(Expr2, [Child|NewChildren], Children, Rest2, Pending, State#state{waiting = Waiting bor Child});

%% add a child call expression
pending_add(Expr = #expr{deps = Deps}, NewChildren, [Child = #expr{type = call}|Children], Rest, Pending, State) ->
  {ID, State2} = next_id(State),
  Expr2 = Expr#expr{deps = Deps bor ID},
  Child2 = Child#expr{id = ID},
  Rest2 = [Child2|Rest],
  Waiting = State#state.waiting,
  pending_add(Expr2, [ID|NewChildren], Children, Rest2, Pending, State2#state{waiting = Waiting bor ID});

%% pass on the literals
pending_add(Expr, NewChildren, [Child = #expr{type = literal}|Children], Rest, Pending, State) ->
  pending_add(Expr, [Child|NewChildren], Children, Rest, Pending, State).

%% resolve all of the needed arguments

resolve_values([], Acc, _) ->
  {ok, lists:reverse(Acc)};
resolve_values([Child|Children], Acc, Values) when is_integer(Child) ->
  Value = maps:get(Child, Values),
  resolve_values(Children, [Value|Acc], Values);
resolve_values([#expr{type = literal, value = Value}|Children], Acc, Values) ->
  resolve_values(Children, [Value|Acc], Values).

pending_alias_value(Value, Expr, Rest, Pending, State) ->
  case Expr#expr.is_root of
    true ->
      {ok, Value, State};
    _ ->
      State2 = set_result(Value, Expr, State),
      pending_loop(Rest, Pending, State2)
  end.

pending_add_comprehension([], _, _, State, Acc) ->
  {ok, lists:reverse(Acc), State};
pending_add_comprehension([Value|Rest], Var, Expr, State, Acc) ->
  Literal = #expr{type = literal, value = Value},
  {ok, [Expr2]} = replace_variable(Var, Literal, [Expr], []),
  pending_add_comprehension(Rest, Var, Expr, State, [Expr2|Acc]).

replace_variable(_, _, [], Acc) ->
  {ok, lists:reverse(Acc)};
replace_variable(Var, Value, [#expr{type = variable, value = Var}|Rest], Acc) ->
  replace_variable(Var, Value, Rest, [Value|Acc]);
replace_variable(Var, Value, [Expr = #expr{children = Children}|Rest], Acc) ->
  {ok, Children2} = replace_variable(Var, Value, Children, []),
  replace_variable(Var, Value, Rest, [Expr#expr{children = Children2}|Acc]);
replace_variable(Var, Value, [Expr|Rest], Acc) ->
  replace_variable(Var, Value, Rest, [Expr|Acc]).

%%%%%%%
%% receive loop.
%%%%%%%

receive_loop(State) ->
  receive
    {ok, Value, Ref} ->
      receive_loop(set_result(Value, Ref, State));
    {error, Error, _Ref} ->
      {error, Error, State}
  after 0 ->
    {ok, State}
  end.

%%%%%%%
%% exec loop.
%%%%%%%

exec_loop(State = #state{calls = []}) ->
  {ok, State};
exec_loop(State = #state{calls = Calls}) ->
  exec_loop(Calls, [], State).

exec_loop([], Calls, State) ->
  {ok, State#state{calls = Calls}};
exec_loop([Expr = #expr{value = {Mod, Fun}, children = Args, id = ID, is_root = IsRoot}|Rest],
           Calls,
           State = #state{cache = Cache, map = Lookup, context = Context, ref = Ref}) ->
  %% TODO spawn
  %% TODO async functions
  CacheKey = {Mod, Fun, Args},
  Hits = State#state.cache_hits,
  case maps:find(CacheKey, Cache) of
    {ok, Value} when IsRoot ->
      {ok, Value, State#state{cache_hits = Hits + 1}};
    {ok, Value} ->
      exec_loop(Rest, Calls, set_result(Value, Expr, State#state{cache_hits = Hits + 1}));
    _ ->
      case Lookup(Mod, Fun, Args, Context, self(), {Ref, ID}) of
        {ok, Value} when IsRoot ->
          {ok, Value, State};
        {ok, Value} ->
          Cache2 = maps:put(CacheKey, Value, Cache),
          exec_loop(Rest, Calls, set_result(Value, Expr, State#state{cache = Cache2}));
        {ok, Value, Context2} ->
          Cache2 = maps:put(CacheKey, Value, Cache),
          exec_loop(Rest, Calls, set_result(Value, Expr, State#state{cache = Cache2, context = Context2}));
        Error ->
          Error
      end
  end.

%%%%%%%
%% error handler.
%%%%%%%

%% TODO cleanup any pending things here
handle_error(Error, State) ->
  {error, Error, State}.

%%%%%%%
%% utils.
%%%%%%%

%% TODO clear the pid
set_result(Value, #expr{id = ID}, State) ->
  set_result(Value, ID, State);
set_result(Value, ID, State = #state{values = Values, completed = Completed}) when is_integer(ID) ->
  Values2 = maps:put(ID, Value, Values),
  State#state{values = Values2, completed = Completed bor ID}.


next_id(State = #state{counter = Counter}) ->
  {trunc(math:pow(2, Counter)), State#state{counter = Counter + 1}}.
