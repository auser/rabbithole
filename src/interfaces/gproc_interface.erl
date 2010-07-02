-module (gproc_interface).
-export ([
  subscribe/1,
  publish/1
]).

-export ([
  init/1,
  stop/0
]).

init([]) -> ok.

subscribe({_QueueName, Props}) -> 
  Fun = get_value(callback, fun unhandled/1, Props),
  gproc:reg({p, 1, {Fun}}, []).

publish({QueueName, Msg, _Props}) ->
  gproc:publish(QueueName, Msg).

unhandled(Msg) ->
  io:format("Unhandled subscribe got message: ~p~n", [Msg]).

% Just a wrapper around the proplists moduel
get_value(Key, Default, Props) ->
  case proplists:get_value(Key, Props) of
    undefined -> Default;
    E -> E
  end.

stop() -> ok.