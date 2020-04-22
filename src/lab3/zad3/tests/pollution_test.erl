-module(pollution_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

runAllTests() ->
  createMonitorTest(),
  addStationTest(),
  addValueTest(),
  removeValueTest(),
  getFunsTest().

createMonitorTest() ->
  ?assertEqual(#{}, pollution:createMonitor()).

addStationTest() ->
  M1 = pollution:addStation("stacja 1", {5, 10}, pollution:createMonitor()),
  ?assertEqual(#{{station,"stacja 1",{5,10}} => []}, M1),
  ?assertEqual({error,name_already_in_the_monitor}, pollution:addStation("stacja 1", {10, 15}, M1)),
  ?assertEqual({error,coords_already_in_the_monitor}, pollution:addStation("stacja 2", {5, 10}, M1)),
  ?assertEqual({error,station_already_in_the_monitor}, pollution:addStation("stacja 1", {5, 10}, M1)),
  M2 = pollution:addStation("stacja 2", {2, 3}, M1),
  ?assertEqual(#{{station,"stacja 1",{5,10}} => [], {station,"stacja 2",{2,3}} => []}, M2).

addValueTest() ->
  M1 = pollution:addStation("stacja 1", {5, 10}, pollution:createMonitor()),
  M2 = pollution:addStation("stacja 2", {2, 3}, M1),
  ?assertEqual({error,no_such_a_station_in_the_monitor}, pollution:addValue("stacja 10", {{2020,4,7},{17,7,36}}, "T1", 10, M2)),
  M3 = pollution:addValue("stacja 2", {{2020,4,7},{17,7,36}}, "T1", 10, M2),
  ?assertEqual(#{{station,"stacja 1",{5,10}} => [],
                 {station,"stacja 2",{2,3}} => [{reading, {{2020,4,7},{17,7,36}}, "T1", 10}]}, M3),
  M4 = pollution:addValue("stacja 2", {{2020, 4, 7}, {16, 0, 0}}, "T2", 30, M3),
  ?assertEqual(#{{station,"stacja 1",{5,10}} => [],
                 {station,"stacja 2",{2,3}} => [{reading, {{2020, 4, 7}, {16, 0, 0}}, "T2", 30},
                                                {reading, {{2020,4,7},{17,7,36}}, "T1", 10}]}, M4),
  ?assertEqual({error,{value_already_in_the_monitor,30}}, pollution:addValue("stacja 2", {{2020, 4, 7}, {16, 0, 0}}, "T2", 60, M4)),
  M5 = pollution:addValue("stacja 2", {{2020,4,7},{17,8,6}}, "T2", 60, M4),
  M6 = pollution:addValue({5, 10}, {{2020, 4, 7}, {17, 0, 15}}, "T1", 5, M5),
  M7 = pollution:addValue({5, 10}, {{2020, 1, 1}, {17, 0, 15}}, "T2", 10, M6),
  ?assertEqual(#{{station,"stacja 1",{5,10}} => [{reading,{{2020,1,1},{17,0,15}},"T2",10},
                                                 {reading,{{2020,4,7},{17,0,15}},"T1",5}],
                 {station,"stacja 2",{2,3}} => [{reading, {{2020,4,7}, {17,8,6}}, "T2",60},
                                                {reading, {{2020,4,7},{16,0,0}}, "T2",30},
                                                {reading,{{2020,4,7},{17,7,36}},"T1",10}]}, M7).

removeValueTest() ->
  M1 = pollution:addStation("stacja 2", {2, 3}, pollution:createMonitor()),
  M2 = pollution:addValue("stacja 2", {{2020,4,7},{17,7,36}}, "T1", 10, M1),
  M3 = pollution:removeValue("stacja 2", {{2020,4,7},{17,7,36}}, "T1", M2),
  ?assertEqual(#{{station,"stacja 2",{2,3}} => []}, M3),
  ?assertEqual({error, no_such_a_value_in_the_monitor}, pollution:removeValue("stacja 2", {{2020,4,7},{17,7,36}}, "T1", M3)).

getFunsTest() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation("stacja 1", {5, 10}, M),
  M2 = pollution:addStation("stacja 2", {2, 3}, M1),
  M3 = pollution:addValue("stacja 2", {{2020,4,7},{17,7,36}}, "T1", 10, M2),
  M4 = pollution:addValue("stacja 2", {{2020, 4, 7}, {16, 0, 0}}, "T2", 30, M3),
  M5 = pollution:addValue("stacja 2", {{2020,4,7},{17,8,6}}, "T2", 60, M4),
  M6 = pollution:addValue({5, 10}, {{2020, 4, 7}, {17, 0, 15}}, "T1", 5, M5),
  M7 = pollution:addValue({5, 10}, {{2020, 1, 1}, {17, 0, 15}}, "T2", 10, M6),
  M8 = pollution:removeValue({5, 10}, {{2020, 1, 1}, {17, 0, 15}}, "T2", M7),

  ?assertEqual(10, pollution:getOneValue("stacja 2", {{2020,4,7},{17,7,36}}, "T1", M8)),
  ?assertEqual(45.0, pollution:getStationMean({2, 3}, "T2", M8)),
  ?assertEqual(7.5, pollution:getDayMean("T1", {2020, 4, 7}, M8)),
  ?assertEqual(#{"T1" => 2,"T2" => 2}, pollution:getTypeCount(M8)),
  ?assertEqual(#{"T1" => 2,"T2" => 3}, pollution:getTypeCount(M7)),
  ?assertEqual([{"T1",7.5},{"T2",45.0}], pollution:getDaySummary({2020, 4, 7}, M7)).
