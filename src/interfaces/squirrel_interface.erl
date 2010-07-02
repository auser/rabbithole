-module (squirrel_interface).
-export ([
  subscribe/1,
  publish/1
]).

-export ([
  init/1,
  stop/0
]).

init([]) -> 
  application:start(squirrel).

subscribe({QueueName, Props}) -> 
  Fun = get_value(callback, fun unhandled/1, Props),
  squirrel:subscribe(QueueName, Fun).

publish({QueueName, Msg, _Props}) ->
  squirrel:publish(QueueName, Msg).

unhandled(Msg) ->
  io:format("Unhandled subscribe got message: ~p~n", [Msg]).

% Just a wrapper around the proplists moduel
get_value(Key, Default, Props) ->
  case proplists:get_value(Key, Props) of
    undefined -> Default;
    E -> E
  end.

stop() -> ok.