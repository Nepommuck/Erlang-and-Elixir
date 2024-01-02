-module(ping_pong).
-author("pawel").

%% API
-export([start/0, stop/0, play/1]).


%% "Variables":
sleep_time() -> 100.
idle_kill_time() -> 20*1000.


%% "Public":
start() ->
  register(ping, spawn(fun() -> ping_loop(0) end)),
  register(pong, spawn(fun() -> pong_loop() end)).

stop() ->
  ping ! kill,
  pong ! kill.

play(N) ->
  ping ! N.


%% "Private":
ping_loop(Sum) ->
  receive
    N when N =< 0 ->
      stop();
    kill -> ok;
    N ->
      New_sum = Sum + N,
      io:format("PING: ~w; Current sum: ~w ~n", [N, New_sum]),
      timer:sleep(sleep_time()),
      pong ! N-1,
      ping_loop(New_sum)

  after(idle_kill_time()) ->
    ok
  end.

pong_loop() ->
  receive
    N when N =< 0 ->
      stop();
    kill -> ok;
    N ->
      io:format("PONG: ~w ~n", [N]),
      timer:sleep(sleep_time()),
      ping ! N-1,
      pong_loop()
  after(idle_kill_time()) ->
    ok
  end.
