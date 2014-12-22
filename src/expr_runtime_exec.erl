-module(expr_runtime_exec).

-export([loop/1]).
%% private
-export([exec_async/9]).

-compile(inline).
-compile({native, [o3]}).

-include("expr.hrl").

-define(REF(Expr, State), {State#state.ref, Expr#expr.id}).

loop(State = #state{calls = []}) ->
  {ok, State};
loop(State = #state{calls = Calls}) ->
  loop(Calls, [], State).

%% TODO async functions
loop([], Calls, State) ->
  {ok, State#state{calls = Calls}};

loop([Expr = #expr{spawn = Spawn, timeout = Timeout}|Rest], Calls, State) when Spawn orelse Timeout > 0 ->
  Lookup = State#state.map,
  Context = State#state.context,
  {Mod, Fun} = Expr#expr.value,
  Args = Expr#expr.children,
  Attrs = Expr#expr.attrs,

  %% TODO lookup in cache
  _CacheKey = {Mod, Fun, Args},

  RefKey = ?REF(Expr, State),

  {_Pid, Ref} = spawn_monitor(?MODULE, exec_async, [Lookup, Timeout, Mod, Fun, Args, Context, self(), RefKey, Attrs]),
  State2 = add_pid(RefKey, Ref, State),
  loop(Rest, Calls, State2);

loop([Expr = #expr{value = {Mod, Fun}, children = Args, is_root = IsRoot}|Rest],
           Calls,
           State = #state{cache = Cache, map = Lookup, context = Context}) ->

  CacheKey = {Mod, Fun, Args},
  Hits = State#state.cache_hits,
  case maps:find(CacheKey, Cache) of
    % {ok, {'__PENDING__'}} -> % happens when a function is async
    %  TODO
    {ok, Value} when IsRoot ->
      {ok, Value, State#state{cache_hits = Hits + 1}};
    {ok, Value} ->
      State2 = set_result(Value, Expr, State#state{cache_hits = Hits + 1}, Cache),
      loop(Rest, Calls, State2);
    _ ->
      RefKey = ?REF(Expr, State),
      case Lookup(Mod, Fun, Args, Context, self(), RefKey, Expr#expr.attrs) of
        {ok, Pid} when is_pid(Pid) ->
          Ref = monitor(process, Pid),
          State2 = add_pid(RefKey, Ref, State),
          loop(Rest, Calls, State2);
        {ok, Ref} when is_reference(Ref) ->
          State2 = add_pid(RefKey, Ref, State),
          loop(Rest, Calls, State2);
        {ok, Value} when IsRoot ->
          {ok, Value, State};
        {ok, Value} ->
          Cache2 = maps:put(CacheKey, Value, Cache),
          State2 = set_result(Value, Expr, State, Cache2),
          loop(Rest, Calls, State2);
        {ok, Value, Context2} ->
          Cache2 = maps:put(CacheKey, Value, Cache),
          State2 = set_result(Value, Expr, State, Cache2, Context2),
          loop(Rest, Calls, State2);
        {error, Error} ->
          {error, Error, State}
      end
  end.

set_result(Value, Expr, State = #state{values = Values, completed = Completed, waiting = Waiting}, Cache) ->
  {Values2, Completed2, Waiting2} = expr_util:set_result(Value, Expr, Values, Completed, Waiting),
  State#state{cache = Cache, values = Values2, completed = Completed2, waiting = Waiting2}.

set_result(Value, Expr, State = #state{values = Values, completed = Completed, waiting = Waiting}, Cache, Context) ->
  {Values2, Completed2, Waiting2} = expr_util:set_result(Value, Expr, Values, Completed, Waiting),
  State#state{cache = Cache, values = Values2, completed = Completed2, waiting = Waiting2, context = Context}.

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

add_pid(RefKey, Ref, State) ->
  State#state{pids = maps:put(RefKey, Ref, State#state.pids)}.
