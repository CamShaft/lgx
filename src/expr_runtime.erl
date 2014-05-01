-module(expr_runtime).

-export([execute/3]).

-include("expr.hrl").

execute([], _, _) ->
  {ok, undefined};
execute(State, MapFun, Context) ->
  loop(State#state{map = MapFun, context = Context, ref = make_ref()}).

%%%%%%%
%% main loop.
%%%%%%%

loop(State = #state{iterations = 1000}) ->
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
            {ok, ExecState = #state{iterations = Iter}} ->
               loop(ExecState#state{iterations = Iter + 1})
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

%% the root is a literal so just return the value
pending_loop([#expr{type = literal, value = Value, is_root = true}|_], _, State) ->
  {ok, Value, State};

%% set the literal value to the id in #state.values
pending_loop([Expr = #expr{type = literal, value = Value}|Rest], Pending, State) ->
  State2 = set_result(Value, Expr, State),
  pending_loop(Rest, Pending, State2);

%% the dependencies are ready so add the 'pending' function to the 'calls' list
pending_loop([Expr = #expr{type = call, deps = Deps, status = waiting}|Rest],
              Pending,
              State = #state{calls = Calls, completed = Completed, values = Values}) when Deps band Completed =:= Deps ->
  {ok, Children} = resolve_values(Expr#expr.children, [], Values),
  Expr2 = Expr#expr{children = Children},
  pending_loop(Rest, Pending, State#state{calls = [Expr2|Calls]});

%% evaluate the first child in a condition
pending_loop([Expr = #expr{type = 'cond', status = added, children = [Cond|_] = Children}|Rest], Pending, State) ->
  {Rest2, Pending2, State2} = pending_add(Expr#expr{deps = 0, tmp = Children}, [], [Cond], Rest, Pending, State),
  pending_loop(Rest2, Pending2, State2);

%% evaluate the selected branch of the condition
pending_loop([Expr = #expr{type = 'cond', deps = Dep, tmp = Children, status = waiting}|Rest],
              Pending,
              State = #state{completed = Completed, values = Values}) when Dep band Completed =:= Dep ->
  Branch = case maps:get(Dep, Values) of
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
pending_loop([Expr = #expr{type = 'cond', deps = Dep, status = branching}|Rest],
              Pending,
              State = #state{completed = Completed, values = Values}) when Dep band Completed =:= Dep ->
  Value = maps:get(Dep, Values),
  case Expr#expr.is_root of
    true ->
      {ok, Value, State};
    _ ->
      State2 = set_result(Value, Expr, State),
      pending_loop(Rest, Pending, State2)
  end;

%% the dependencies have not been added to the pending list
pending_loop([Expr = #expr{children = Children, status = added, deps = -1}|Rest], Pending, State)  ->
  {Rest2, Pending2, State2} = pending_add(Expr#expr{deps = 0}, [], Children, Rest, Pending, State),
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
pending_add(Expr = #expr{deps = Deps}, NewChildren, [Child = #expr{type = call}|Children], Rest, Pending, State = #state{waiting = Waiting}) ->
  {ID, State2} = next_id(State),
  Expr2 = Expr#expr{deps = Deps bor ID},
  Child2 = Child#expr{id = ID},
  Rest2 = [Child2|Rest],
  pending_add(Expr2, [ID|NewChildren], Children, Rest2, Pending, State2#state{waiting = Waiting bor ID});

%% continue on... REMOVE THIS ONCE ALL IS READY!!! - there should be a match for all previous patterns or crash
pending_add(Expr, NewChildren, [Child|Children], Rest, Pending, State) ->
  pending_add(Expr, [Child|NewChildren], Children, Rest, Pending, State).

%% resolve all of the needed arguments

resolve_values([], Acc, _) ->
  {ok, lists:reverse(Acc)};
resolve_values([Child|Children], Acc, Values) when is_integer(Child) ->
  Value = maps:get(Child, Values),
  resolve_values(Children, [Value|Acc], Values);
resolve_values([#expr{type = literal, value = Value}|Children], Acc, Values) ->
  resolve_values(Children, [Value|Acc], Values).

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
set_result(Value, #expr{id = ID}, State = #state{values = Values, completed = Completed}) ->
  Values2 = maps:put(ID, Value, Values),
  State#state{values = Values2, completed = Completed bor ID}.

next_id(State = #state{counter = Counter}) ->
  {trunc(math:pow(2, Counter)), State#state{counter = Counter + 1}}.
