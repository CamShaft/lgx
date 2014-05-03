-module(expr_runtime).

-export([execute/3]).

%% private
-export([exec_async/9]).

-include("expr.hrl").

execute(State, MapFun, Context) ->
  loop(State#state{map = MapFun, context = Context, ref = make_ref()}).

%%%%%%%
%% main loop.
%%%%%%%

loop(State = #state{stalled = 100}) ->
  {error, infinite_loop, State};
loop(State) ->
  case expr_runtime_pending:loop(State) of
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
%% receive loop.
%%%%%%%

receive_loop(State) ->
  Ref = State#state.ref,
  receive
    {ok, Value, {Ref, ID}} when is_integer(ID) ->
      State2 = expr_util:set_result(Value, ID, State),
      receive_loop(State2);
    {error, Error, _Ref} ->
      {error, Error, State}
    %% {'DOWN', _ChildRef, _, _, normal} ->
    %%   %% TODO remove from pids
    %%   receive_loop(State);
    %% Error ->
    %%   {error, Error, State}
  after 0 ->
    {ok, State}
    %% case length(State#state.pids) of
    %%   0 ->
    %%     {ok, State};
    %%   _ when length(State#state.calls) > 0 ->
    %%     {ok, State};
    %%   _ ->
    %%     receive
    %%       {ok, Value, {Ref, ID}} ->
    %%         State2 = set_result(Value, ID, State),
    %%         receive_loop(State2);
    %%       {'DOWN', _ChildRef, _, _, normal} ->
    %%         %% TODO remove from pids
    %%         receive_loop(State);
    %%       Error ->
    %%         {error, Error, State}
    %%     end
    %% end
  end.

%%%%%%%
%% exec loop.
%%%%%%%

exec_loop(State = #state{calls = []}) ->
  {ok, State};
exec_loop(State = #state{calls = Calls}) ->
  exec_loop(Calls, [], State).

%% TODO async functions
exec_loop([], Calls, State) ->
  {ok, State#state{calls = Calls}};

exec_loop([Expr = #expr{spawn = Spawn, timeout = Timeout}|Rest], Calls, State) when Spawn orelse Timeout > 0 ->
  ReqRef = State#state.ref,
  Lookup = State#state.map,
  Context = State#state.context,
  {Mod, Fun} = Expr#expr.value,
  Args = Expr#expr.children,
  ID = Expr#expr.id,
  Attrs = Expr#expr.attrs,

  %% TODO lookup in cache
  _CacheKey = {Mod, Fun, Args},

  {_Pid, Ref} = spawn_monitor(?MODULE, exec_async, [Lookup, Timeout, Mod, Fun, Args, Context, self(), {ReqRef, ID}, Attrs]),
  Pids = [{Expr, Ref}|State#state.pids],
  exec_loop(Rest, Calls, State#state{pids = Pids});

exec_loop([Expr = #expr{value = {Mod, Fun}, children = Args, id = ID, is_root = IsRoot}|Rest],
           Calls,
           State = #state{cache = Cache, map = Lookup, context = Context, ref = ReqRef}) ->

  CacheKey = {Mod, Fun, Args},
  Hits = State#state.cache_hits,
  case maps:find(CacheKey, Cache) of
    % {ok, {'__PENDING__'}} -> % happens when a function is async
    %  TODO
    {ok, Value} when IsRoot ->
      {ok, Value, State#state{cache_hits = Hits + 1}};
    {ok, Value} ->
      State2 = expr_util:set_result(Value, Expr, State#state{cache_hits = Hits + 1}),
      exec_loop(Rest, Calls, State2);
    _ ->
      case Lookup(Mod, Fun, Args, Context, self(), {ReqRef, ID}, Expr#expr.attrs) of
        {ok, Pid} when is_pid(Pid) ->
          Ref = monitor(process, Pid),
          Pids = [{Expr, Ref}|State#state.pids],
          exec_loop(Rest, Calls, State#state{pids = Pids});
        {ok, Ref} when is_reference(Ref) ->
          Pids = [{Expr, Ref}|State#state.pids],
          exec_loop(Rest, Calls, State#state{pids = Pids});
        {ok, Value} when IsRoot ->
          {ok, Value, State};
        {ok, Value} ->
          Cache2 = maps:put(CacheKey, Value, Cache),
          State2 = expr_util:set_result(Value, Expr, State#state{cache = Cache2}),
          exec_loop(Rest, Calls, State2);
        {ok, Value, Context2} ->
          Cache2 = maps:put(CacheKey, Value, Cache),
          State2 = expr_util:set_result(Value, Expr, State#state{cache = Cache2, context = Context2}),
          exec_loop(Rest, Calls, State2);
        {error, Error} ->
          {error, Error, State}
      end
  end.

exec_async(Lookup, Timeout, Mod, Fun, Args, Context, Parent, Ref, Attrs) when Timeout > 0 ->
  {ok, Tref} = timer:kill_after(Timeout),
  exec_async(Lookup, Tref, Mod, Fun, Args, Context, Parent, Ref, Attrs);
exec_async(Lookup, Tref, Mod, Fun, Args, Context, Parent, Ref, Attrs) ->
  Result = Lookup(Mod, Fun, Args, Context, Parent, Ref, Attrs),

  %% ignore the error if it's not an TRef since there's no easy way to check
  %% http://www.erlang.org/doc/man/timer.html
  timer:cancel(Tref),

  Parent ! case Result of
    {ok, Value} ->
      {ok, Value, Ref};
    {ok, Value, Context} ->
      {ok, Value, Context, Ref};
    {error, Reason} ->
      {error, Reason, Ref};
    {error, Reason, Context} ->
      {error, Reason, Context, Ref};
    _ ->
      {error, {invalid_return, Result}, Ref}
  end.

%%%%%%%
%% error handler.
%%%%%%%

%% TODO cleanup any pending things here
handle_error(Error, State) ->
  {error, Error, State}.
