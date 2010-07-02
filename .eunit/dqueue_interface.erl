-module (dqueue_interface).
-export ([
  subscribe/2,
  publish/3
]).

-export ([init/1]).

init([]) -> ok.

subscribe(QueueName) -> subscribe(QueueName, []).
subscribe(QueueName, Props) -> 
  Fun = get_value(callback, fun unhandled/1, Props),
  dqueue:subscribe(QueueName, Fun).

publish(QueueName, Msg) -> publish(QueueName, Msg, []).
publish(QueueName, Msg, _Props) ->
  dqueue:publish(QueueName, Msg).

unhandled(Msg) ->
  io:format("Unhandled subscribe got message: ~p~n", [Msg]).

% Just a wrapper around the proplists moduel
get_value(Key, Default, Props) ->
  case proplists:get_value(Key, Props) of
    undefined -> Default;
    E -> E
  end.