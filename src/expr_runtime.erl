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

pending_loop(State = #state{pending = Pending}) ->
  pending_loop(Pending, [], State).

pending_loop([], Pending, State) ->
  {ok, State#state{pending = Pending}};

%% the root is a literal so just return the value
pending_loop([#expr{type = literal, value = Value, is_root = true}|_], _, State) ->
  {ok, Value, State};

%% set the literal value to the id in #state.values
%% pending_loop()

%% clear the completed functions
pending_loop([#expr{id = ID}|Rest], Pending, State = #state{completed = Completed}) when ID band Completed =:= ID ->
  pending_loop(Rest, Pending, State);

%% the dependencies are ready so add the 'pending' function to the 'calls' list
pending_loop([Expr = #expr{type = call, deps = Deps, status = waiting}|Rest],
              Pending,
              State = #state{calls = Calls, completed = Completed, values = Values}) when Deps band Completed =:= Deps ->
  Args = [maps:get(Key, Values) || Key <- Expr#expr.children],
  Expr2 = Expr#expr{children = Args},
  pending_loop(Rest, [Expr#expr{status = in_progress}|Pending], State#state{calls = [Expr2|Calls]});

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

%% add a child literal expression
%% pending_add(Expr = #expr{deps = Deps}, NewChildren, [Child = #expr{type = literal}|Children], Rest, Pending, State) ->
%%   {ID, State2} = next_id(State),
%%   Expr2 = Expr#expr{deps = Deps bor ID},
  
%%   pending_add(Expr2, [ID|NewChildren], Children, Rest2, Pending2, State2);

pending_add(Expr, NewChildren, [_|Children], Rest, Pending, State) ->
  pending_add(Expr, NewChildren, Children, Rest, Pending, State).

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

exec_loop(State = #state{calls = Calls}) ->
  exec_loop(Calls, [], State).

exec_loop([], Calls, State) ->
  {ok, State#state{calls = Calls}};
exec_loop([Expr = #expr{value = {Mod, Fun}, children = Args, id = ID, is_root = IsRoot}|Rest],
           Calls,
           State = #state{cache = Cache, map = Lookup, context = Context, ref = Ref}) ->

  CacheKey = {Mod, Fun, Args},
  case maps:find(CacheKey, Cache) of
    {ok, Value} when IsRoot ->
      {ok, Value, State};
    {ok, Value} ->
      exec_loop(Rest, Calls, set_result(Value, Expr, State));
    _ ->
      case Lookup(Mod, Fun, Args, Context, self(), {Ref, ID}) of
        {ok, Value} when IsRoot ->
          {ok, Value, State};
        {ok, Value} ->
          Cache2 = maps:put(CacheKey, Value, Cache),
          exec_loop(Rest, Calls, set_result(Value, Expr, State#state{cache = Cache2}));
        Error ->
          Error
      end
  end.

%% cleanup any pending things here
handle_error(Error, State) ->
  {error, Error, State}.

%% utils.

%% TODO clear the pid
set_result(Value, #expr{id = ID}, State = #state{values = Values, completed = Completed}) ->
  Values2 = maps:put(ID, Value, Values),
  State#state{values = Values2, completed = Completed bor ID}.

next_id(State = #state{counter = Counter}) ->
  {trunc(math:pow(2, Counter)), State#state{counter = Counter + 1}}.
