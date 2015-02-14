-module(expr_runtime_exec).

-export([loop/1]).
%% private
-export([exec_async/9]).

-compile({native, [o3]}).

-include("expr.hrl").

-define(REF(ID, StateRef), {StateRef, ID}).

loop(State = #state{calls = []}) ->
  {ok, State};
loop(State = #state{calls = Calls,
                    cache = Cache,
                    cache_hits = Hits,
                    map = Lookup,
                    context = Context,
                    values = Values,
                    completed = Completed,
                    waiting = Waiting,
                    pids = Pids,
                    ref = StateRef}) ->
  loop(Calls, [], Cache, Hits, Lookup, Context, Values, Completed, Waiting, Pids, StateRef, State).

loop([], Calls, Cache, Hits, _Lookup, Context, Values, Completed, Waiting, Pids, _StateRef, State) ->
  {ok, State#state{calls = Calls,
                   cache = Cache,
                   cache_hits = Hits,
                   context = Context,
                   values = Values,
                   completed = Completed,
                   waiting = Waiting,
                   pids = Pids}};

loop([#expr{id = ID, value = {Mod, Fun}, children = Args, attrs = Attrs, spawn = Spawn, timeout = Timeout}|Rest],
      Calls, Cache, Hits, Lookup, Context, Values, Completed, Waiting, Pids, StateRef, State) when Spawn orelse Timeout > 0 ->
  %% TODO lookup in cache
  %% _CacheKey = {Mod, Fun, Args},

  RefKey = ?REF(ID, StateRef),

  {_Pid, Ref} = spawn_monitor(?MODULE, exec_async, [Lookup, Timeout, Mod, Fun, Args, Context, self(), RefKey, Attrs]),
  Pids2 = maps:put(RefKey, Ref, Pids),
  loop(Rest, Calls, Cache, Hits, Lookup, Context, Values, Completed, Waiting, Pids2, StateRef, State);

loop([#expr{native = true, id = ID, value = {Mod, Fun}, children = Args, is_root = IsRoot}|Rest],
      Calls, Cache, Hits, Lookup, Context, Values, Completed, Waiting, Pids, StateRef, State) ->
  case apply(Mod, Fun, Args) of
    Value when IsRoot ->
      {ok, Value, State#state{cache_hits = Hits + 1, context = Context}};
    Value ->
      {Values2, Completed2, Waiting2} = expr_util:set_result(Value, ID, Values, Completed, Waiting),
      loop(Rest, Calls, Cache, Hits + 1, Lookup, Context, Values2, Completed2, Waiting2, Pids, StateRef, State)
  end;

loop([#expr{id = ID, value = {Mod, Fun}, children = Args, attrs = Attrs, is_root = IsRoot}|Rest],
      Calls, Cache, Hits, Lookup, Context, Values, Completed, Waiting, Pids, StateRef, State) ->

  CacheKey = {Mod, Fun, Args},
  case maps:find(CacheKey, Cache) of
    % {ok, {'__PENDING__'}} -> % happens when a function is async
    %  TODO
    {ok, Value} when IsRoot ->
      {ok, Value, State#state{cache_hits = Hits + 1, context = Context}};
    {ok, Value} ->
      {Values2, Completed2, Waiting2} = expr_util:set_result(Value, ID, Values, Completed, Waiting),
      loop(Rest, Calls, Cache, Hits + 1, Lookup, Context, Values2, Completed2, Waiting2, Pids, StateRef, State);
    _ ->
      RefKey = ?REF(ID, State),
      case Lookup(Mod, Fun, Args, Context, self(), RefKey, Attrs) of
        {ok, Pid} when is_pid(Pid) ->
          Ref = monitor(process, Pid),
          Pids2 = maps:put(RefKey, Ref, Pids),
          loop(Rest, Calls, Cache, Hits, Lookup, Context, Values, Completed, Waiting, Pids2, StateRef, State);
        {ok, Ref} when is_reference(Ref) ->
          Pids2 = maps:put(RefKey, Ref, Pids),
          loop(Rest, Calls, Cache, Hits, Lookup, Context, Values, Completed, Waiting, Pids2, StateRef, State);
        {ok, Value} when IsRoot ->
          {ok, Value, State};
        {ok, Value} ->
          Cache2 = maps:put(CacheKey, Value, Cache),
          {Values2, Completed2, Waiting2} = expr_util:set_result(Value, ID, Values, Completed, Waiting),
          loop(Rest, Calls, Cache2, Hits, Lookup, Context, Values2, Completed2, Waiting2, Pids, StateRef, State);
        {ok, Value, Context2} ->
          Cache2 = maps:put(CacheKey, Value, Cache),
          {Values2, Completed2, Waiting2} = expr_util:set_result(Value, ID, Values, Completed, Waiting),
          loop(Rest, Calls, Cache2, Hits, Lookup, Context2, Values2, Completed2, Waiting2, Pids, StateRef, State);
        {error, Error} ->
          {error, Error, State}
      end
  end.

exec_async(Lookup, Timeout, Mod, Fun, Args, Context, Parent, Ref, Attrs) when is_integer(Timeout) andalso Timeout > 0 ->
  Tref = timer:kill_after(Timeout),
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
