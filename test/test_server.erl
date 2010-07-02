-module (test_server).

-export ([
  start/0,
  add/1,
  get_value/0,
  stop/0
]).

start() -> 
  Pid = spawn_link(fun loop/0),
  register(?MODULE, Pid),
  {ok, Pid}.
  
add(N) -> whereis(?MODULE) ! {add, N}.
get_value()  -> 
  whereis(?MODULE) ! {get_value, self()},
  receive
    {value, V} -> V
  end.
stop()  -> whereis(?MODULE) ! {stop}.

loop() -> loop(0).
loop(N) ->
  receive
    {add, I} ->
      NewN = N + I,
      loop(NewN);
    {get_value, From} ->
      From ! {value, N},
      loop(N);
    {stop} ->
      ok;
    _X ->
      loop(N)
  end.