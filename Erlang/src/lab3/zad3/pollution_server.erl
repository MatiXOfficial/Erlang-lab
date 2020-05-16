-module(pollution_server).

-export([start/0, stop/0, getMonitor/0, addStation/2, addValue/4, removeValue/3, getOneValue/3,
         getStationMean/2, getDayMean/2, getTypeCount/0, getDaySummary/1]).


start() ->
  Pid = spawn(fun() -> init() end),
  register(pollution_server, Pid),
  ok.

stop() ->
  pollution_server ! stop,
  unregister(pollution_server),
  ok.

getMonitor() ->
  pollution_server ! {getMonitor, self()},
  receive
    {getMonitor, M} ->
      M
  end.

addStation(Name, {X, Y}) ->
  pollution_server ! {addStation, {Name, {X, Y}}, self()},
  handleReturn().

addValue(Arg, Date, What, Value) ->
  pollution_server ! {addValue, {Arg, Date, What, Value}, self()},
  handleReturn().

removeValue(Arg, Date, What) ->
  pollution_server ! {removeValue, {Arg, Date, What}, self()},
  handleReturn().

getOneValue(Arg, Date, What) ->
  pollution_server ! {getOneValue, {Arg, Date, What}, self()},
  handleReturn().

getStationMean(Arg, What) ->
  pollution_server ! {getStationMean, {Arg, What}, self()},
  handleReturn().

getDayMean(What, Day) ->
  pollution_server ! {getDayMean, {What, Day}, self()},
  handleReturn().

getTypeCount() ->
  pollution_server ! {getTypeCount, {}, self()},
  handleReturn().

getDaySummary(Day) ->
  pollution_server ! {getDaySummary, {Day}, self()},
  handleReturn().


%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
  loop(#{}).

loop(M) ->
  receive
    stop -> ok;
    {getMonitor, Pid} ->
      Pid ! {getMonitor, M},
      loop(M);
    {addStation, {Name, {X, Y}}, Pid} ->
      loopHelper(Pid, M, {new, pollution:addStation(Name, {X, Y}, M)});
    {addValue, {Arg, Date, What, Value}, Pid} ->
      loopHelper(Pid, M, {new, pollution:addValue(Arg, Date, What, Value, M)});
    {removeValue, {Arg, Date, What}, Pid} ->
      loopHelper(Pid, M, {new, pollution:removeValue(Arg, Date, What, M)});
    {getOneValue, {Arg, Date, What}, Pid} ->
      loopHelper(Pid, M, {res, pollution:getOneValue(Arg, Date, What, M)});
    {getStationMean, {Arg, What}, Pid} ->
      loopHelper(Pid, M, {res, pollution:getStationMean(Arg, What, M)});
    {getDayMean, {What, Day}, Pid} ->
      loopHelper(Pid, M, {res, pollution:getDayMean(What, Day, M)});
    {getTypeCount, {}, Pid} ->
      loopHelper(Pid, M, {res, pollution:getTypeCount(M)});
    {getDaySummary, {Day}, Pid} ->
      loopHelper(Pid, M, {res, pollution:getDaySummary(Day, M)})
  end.

loopHelper(Pid, OldM, {_, {error, Mes}}) ->
  Pid ! {error, Mes},
  loop(OldM);
loopHelper(Pid, _, {new, NewM}) ->
  Pid ! ok,
  loop(NewM);
loopHelper(Pid, M, {res, Res}) ->
  Pid ! Res,
  loop(M).

handleReturn() ->
  receive
    ok -> ok;
    Res -> Res
  end.