-module(expr_runtime).

-export([execute/3]).

-compile(inline).
-compile({native, [o3]}).

-include("expr.hrl").

execute(State, MapFun, Context) ->
  loop(State#state{map = MapFun, context = Context, ref = make_ref()}).

%%%%%%%
%% main loop.
%%%%%%%

loop(State = #state{stalled = 100}) ->
  {error, infinite_loop, State};
loop(State) ->
  ?DEBUG("starting run loop~n"),
  case expr_runtime_pending:loop(State) of
    %% we're done
    {ok, Value, PendingState} ->
      {ok, Value, PendingState};
    {error, Error, PendingState} ->
      handle_error(Error, PendingState);
    {ok, PendingState} ->
      case expr_runtime_receive:loop(PendingState) of
        {error, Error, ReceiveState} ->
          handle_error(Error, ReceiveState);
        {ok, ReceiveState} ->
          case expr_runtime_exec:loop(ReceiveState) of
            {error, Error, ExecState} ->
              handle_error(Error, ExecState);
            %% we're done!
            {ok, Value, ExecState} ->
              {ok, Value, ExecState};
            {ok, State = #state{iterations = Iter, stalled = Stalled}} ->
              ?DEBUG("stalled iteration ~p~n", [Iter + 1]),
              loop(State#state{iterations = Iter + 1, stalled = Stalled + 1});
            {ok, ExecState = #state{iterations = Iter}} ->
              ?DEBUG("iteration ~p~n", [Iter + 1]),
              loop(ExecState#state{iterations = Iter + 1, stalled = 0})
          end
      end
  end.

%%%%%%%
%% error handler.
%%%%%%%

%% TODO cleanup any pending things here
handle_error(Error, State) ->
  ?DEBUG("got an error! ~p~n", [Error]),
  {error, Error, State}.
