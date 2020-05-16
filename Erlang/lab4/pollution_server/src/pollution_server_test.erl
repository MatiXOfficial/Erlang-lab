-module(pollution_server_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

runAllTests() ->
  stateAndResultTest().

stateAndResultTest() ->
  ?assertEqual(ok, pollution_gen_server:addStation("stacja 1", {5, 10})),
  ?assertEqual({error, station_already_in_the_monitor}, pollution_gen_server:addStation("stacja 1", {5, 10})),
  pollution_gen_server:addStation("stacja 2", {2, 3}),
  ?assertEqual(#{{station,"stacja 1",{5,10}} => [], {station,"stacja 2",{2,3}} => []}, pollution_gen_server:getMonitor()),

  pollution_gen_server:addValue("stacja 2", {{2020,4,7},{17,7,36}}, "T1", 10),
  pollution_gen_server:addValue("stacja 2", {{2020, 4, 7}, {16, 0, 0}}, "T2", 30),
  pollution_gen_server:addValue("stacja 2", {{2020,4,7},{17,8,6}}, "T2", 60),
  pollution_gen_server:addValue({5, 10}, {{2020, 4, 7}, {17, 0, 15}}, "T1", 5),
  pollution_gen_server:addValue({5, 10}, {{2020, 1, 1}, {17, 0, 15}}, "T2", 10),
  ?assertEqual(#{{station,"stacja 1",{5,10}} => [{reading,{{2020,1,1},{17,0,15}},"T2",10},
    {reading,{{2020,4,7},{17,0,15}},"T1",5}],
    {station,"stacja 2",{2,3}} => [{reading, {{2020,4,7}, {17,8,6}}, "T2",60},
      {reading, {{2020,4,7},{16,0,0}}, "T2",30},
      {reading,{{2020,4,7},{17,7,36}},"T1",10}]}, pollution_gen_server:getMonitor()),

  ?assertEqual(10, pollution_gen_server:getOneValue("stacja 2", {{2020,4,7},{17,7,36}}, "T1")),
  ?assertEqual(45.0, pollution_gen_server:getStationMean({2, 3}, "T2")),
  ?assertEqual(7.5, pollution_gen_server:getDayMean("T1", {2020, 4, 7})),
  ?assertEqual(#{"T1" => 2,"T2" => 3}, pollution_gen_server:getTypeCount()),
  ?assertEqual([{"T1",7.5},{"T2",45.0}], pollution_gen_server:getDaySummary({2020, 4, 7})).