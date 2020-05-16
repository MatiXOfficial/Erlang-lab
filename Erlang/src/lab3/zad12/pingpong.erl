-module(pingpong).

-export([start/0, stop/0, play/1]).

start() ->
  Pid1 = spawn(fun() -> pingLoop(0) end),
  Pid2 = spawn(fun() -> pongLoop() end),
  register(ping, Pid1),
  register(pong, Pid2),
  ok.

stop() ->
  ping ! stop,
  pong ! stop,
  unregister(ping),
  unregister(pong),
  ok.

play(N) ->
  ping ! N.

pingLoop(Count) ->
  receive
    stop -> ok;
    1 ->
      io:format("Ping odebral 1~n"),
      pingLoop(Count + 1);
    N ->
      io:format("Ping odebral ~B, suma = ~B~n", [N, Count + N]),
      timer:sleep(200),
      pong ! N - 1,
      pingLoop(Count + N)
  after 20000 ->
    ok
  end.

pongLoop() ->
  receive
    stop -> ok;
    1 ->
      io:format("Pong odebral 1~n"),
      pongLoop();
    N when N > 1 ->
      io:format("Pong odebral ~B~n", [N]),
      timer:sleep(200),
      ping ! N - 1,
      pongLoop()
  after 20000 ->
    ok
  end.