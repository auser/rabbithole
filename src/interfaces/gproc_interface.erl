-module (gproc_interface).
-include_lib("stdlib/include/qlc.hrl").
-export ([
  subscribe/1,
  publish/1,
  list/1
]).

-export ([
  init/1,
  stop/0,
  unhandled/1
]).

init([]) -> ok.

list(queues) ->
  Q = qlc:q([ QueueName || {{p, l, {QueueName, _Fun}}, _Pid, _} <- gproc:table(props) ]),
  qlc:eval(Q).

subscribe({QueueName, Props}) when is_list(QueueName) -> subscribe({erlang:list_to_atom(QueueName), Props});
subscribe({QueueName, Props}) -> 
  Fun = get_value(callback, fun unhandled/1, Props),
  gproc:reg({p, l, {QueueName, Fun}}).

publish({QueueName, Msg, Props}) when is_list(QueueName) -> publish({erlang:list_to_atom(QueueName), Msg, Props});
publish({QueueName, Msg, _Props}) ->
  % gproc:publish(QueueName, Msg).
  Q = qlc:q([
    case QueueName =:= ToQueue of
      false -> ok;
      true ->
        rpc:cast(node(Pid), erlang, apply, [Fun, [{msg, erlang:term_to_binary(Msg)}]])
    end
    || {{p, l, {ToQueue, Fun}}, Pid, _} <- gproc:table(props) 
  ]),
  qlc:eval(Q).

unhandled(Msg) ->
  io:format("Unhandled subscribe got message: ~p~n", [Msg]).

% Just a wrapper around the proplists moduel
get_value(Key, Default, Props) ->
  case proplists:get_value(Key, Props) of
    undefined -> Default;
    E -> E
  end.

stop() -> ok.