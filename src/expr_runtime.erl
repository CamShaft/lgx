-module(expr_runtime).

-export([execute/3]).

-include("expr.hrl").

-record(state, {
  values :: map(),
  exprs = [] :: list(),
  pending = [] :: list(),
  counter = 0 :: integer(),
  completed = 0 :: integer(),
  errors = [] :: list(),
  map :: fun(),
  context :: any(),
  iterations = 0 :: integer(),
  pids = [] :: list()
}).

-define(MOCK, #state{
  values = #{

  },
  %% all of the initial expressions
  exprs = #{

  },
  pending = [
    #{
      type => literal,
      value => atom
    }
  ],
  map = fun() ->
    {ok, todo}
  end
}).

execute([], _, _) ->
  {ok, undefined};
execute(_Forms, _MapFun, _Context) ->
  loop(?MOCK).

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
          case timeout_loop(ReceiveState) of
            {error, Error, TimeoutState} ->
              handle_error(Error, TimeoutState);
            {ok, TimeoutState} ->
              case exec_loop(TimeoutState) of
                {error, Error, ExecState} ->
                  handle_error(Error, ExecState);
                {ok, ExecState = #state{iterations = Iter}} ->
                  loop(ExecState#state{iterations = Iter + 1})
              end
          end
      end
  end.

%% pending loop.

pending_loop(State = #state{pending = _Pending}) ->
  {ok, State}.
  %% pending_loop(Pending, State).

%% pending_loop([#{type => literal}|Pending], State) ->

receive_loop(State) ->
  receive
    {ok, Value, Ref} ->
      receive_loop(set_result(Value, Ref, State));
    {error, Error, _Ref} ->
      {error, Error}
  after 0 ->
    {ok, State}
  end.

%% timeout loop.

timeout_loop(State = #state{pids = Pids}) ->
  timeout_loop(Pids, get_time(), State).

timeout_loop([], _, State) ->
  {ok, State};
timeout_loop([{Start, Timeout, Silent, Ref}|Pids], Now, State) when Now - Start > Timeout ->
  case Silent of
    true ->
      timeout_loop(Pids, Now, State);
    _ ->
      {error, {timeout, Ref}}
  end;
timeout_loop([_|Pids], Now, State) ->
  timeout_loop(Pids, Now, State).

%% exec loop.

exec_loop(State) ->
  {ok, State}.

%% cleanup any pending things here
handle_error(Error, State) ->
  {error, Error, State}.

%% utils.

%% TODO clear the pid
%% TODO set the result in `values`
set_result(_Value, _Ref, State) ->
  State.

get_time() ->
  0.
