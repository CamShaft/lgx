-module(expr_runtime_receive).

-export([loop/1]).

-compile({native, [o3]}).

-include("expr.hrl").

-define(RECEIVE(Ref, State),
  {ok, Value, {Ref, ID} = RefKey} when is_integer(ID) ->
    State2 = expr_util:set_result(Value, ID, State),
    loop(remove_from_pids(RefKey, State2));
  {'DOWN', _ChildRef, _, _, normal} ->
    %% TODO remove from pids
    loop(State);
  {'DOWN', _ChildRef, _, _, Reason} ->
    {error, Reason, State};
  {error, Reason, {Ref, ID}} when is_integer(ID) ->
    {error, Reason, State};
  {error, Reason, _Context, {Ref, ID}} when is_integer(ID) ->
    %% TODO store context
    {error, Reason, State}
).

loop(State) ->
  Ref = State#state.ref,
  receive
    ?RECEIVE(Ref, State)
  after 0 ->
    case maps:size(State#state.pids) of
      0 ->
        {ok, State};
      _ when length(State#state.calls) > 0 ->
        {ok, State};
      _ ->
        waiting(State)
    end
  end.

waiting(State) ->
  Ref = State#state.ref,
  ?DEBUG("receive::begin~n"),
  Res = receive
    ?RECEIVE(Ref, State)
  end,
  ?DEBUG("receive::end~n"),
  Res.

remove_from_pids(RefKey, State) ->
  ?DEBUG("removing pid ~p~n", [RefKey]),
  Pids = maps:remove(RefKey, State#state.pids),
  State#state{pids = Pids}.
