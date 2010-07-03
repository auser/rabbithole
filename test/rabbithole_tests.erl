-module (rabbithole_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  % application:start(sasl),
  test_server:start(),
  ok.
  
teardown(_X) ->
  test_server:stop(),
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun gproc_basic_tests/0,
        fun gproc_add_worker_tests/0
        % fun rabbitmq_basic_tests/0
      ]
    }
  }.

% Put this back in when we are ready for rabbitmq
% rabbitmq_basic_tests() ->
%   try
%     rabbithole_app:start([], [rabbitmq]),
%     Queue = "a.b.c",
%     ?assert(true =:= rabbithole:subscribe(Queue, [{callback, fun callback/1}])),
%     rabbithole:publish(Queue, erlang:term_to_binary({add, 1})),
%     timer:sleep(10),
%     ?assert(11 == test_server:get_value()),
%     rabbithole:publish(Queue, erlang:term_to_binary({add, 10})),
%     rabbithole:publish(Queue, erlang:term_to_binary({subtract, 1})),
%     timer:sleep(10),
%     erlang:display({test_server:get_value()}),
%     ?assert(20 == test_server:get_value()),
%     rabbithole_app:stop([]),
%     passed
%   catch
%     throw:Error ->
%       Error
%   end.

callback({subscribed, _}) -> ok;
callback({msg, Msg}) ->
  message_callback(binary_to_term(Msg)).

message_callback({add, Int}) -> test_server:add(Int);
message_callback(_Else) -> ok.

tee({msg, _Msg}) ->
  ok.

gproc_basic_tests() ->
  rabbithole_app:start([], [gproc]),
  Queue2 = "pete",
  ?assert(true =:= rabbithole:subscribe(Queue2, [{callback, fun tee/1}])),
  Queue = "a.b.c",
  ?assert(true =:= rabbithole:subscribe(Queue, [{callback, fun callback/1}])),
  rabbithole:publish(Queue, {add, 1}),
  timer:sleep(10),
  ?assert(1 == test_server:get_value()),
  rabbithole:publish(Queue, {add, 10}),
  rabbithole:publish(Queue2, {add, 1}),
  timer:sleep(10),
  ?assert(11 == test_server:get_value()),
  ?assert(['a.b.c', pete] == rabbithole:list(queues)),
  rabbithole_app:stop([]),
  passed.

gproc_add_worker_tests() ->
  rabbithole_app:start([], [gproc]),
  rabbithole:add_worker(fun job_received/1),
  _O = timer:tc(lists, foreach, [fun(_N) ->
    rabbithole:submit_job({time, element(3, now())})
  end, lists:seq(1, 1000)]),
  % erlang:display({o, O}),
  timer:sleep(100),
  rabbithole_app:stop([]),
  passed.

job_received({time, TimePutIn}) ->
  % Do SOME work
  element(3, now()) - TimePutIn;
job_received(_List) ->
  % erlang:display({got, List, element(3, now())}),
  ok.
