%%%-------------------------------------------------------------------
%%% @author rafal
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. kwi 2018 11:18
%%%-------------------------------------------------------------------
-module(pollutionTest).
-include_lib("eunit/include/eunit.hrl").
-author("rafal").

%% API
-export([test/0]).

-record(station, {location, measurements}).
-record(measurement, {type, value, date}).

test() ->
  createMonitor_test(),
  addStationAdding1_test(),
  addStationAdding2_test(),
  addStationAllTheSame_test(),
  addStationTheSameLocation_test(),
  addStationTheSameName_test(),
  addValueByName_test().

createMonitor_test() ->
  ?assertEqual(pollution:createMonitor(), #{}).

addStationAdding1_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}},
  M2 = pollution:addStation(pollution:createMonitor(), "Krakow", {100, 10}),
  ?assertEqual(M1, M2).

addStationAdding2_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addStation(pollution:createMonitor(), "Krakow", {100, 10}),
  M3 = pollution:addStation(M2, "Poznan", {150, 30}),
  ?assertEqual(M3, M1).

addStationAllTheSame_test() ->
  M1 = pollution:addStation(pollution:createMonitor(), "Krakow", {100, 10}),
  M2 = pollution:addStation(M1, "Krakow", {100, 10}),
  ?assertEqual(M1, M2).

addStationTheSameLocation_test() ->
  M1 = pollution:addStation(pollution:createMonitor(), "Krakow", {100, 10}),
  M2 = pollution:addStation(M1, "Krakw", {100, 10}),
  ?assertEqual(M1, M2).

addStationTheSameName_test() ->
  M1 = pollution:addStation(pollution:createMonitor(), "Krakow", {100, 10}),
  M2 = pollution:addStation(M1, "Krakow", {10, 10}),
  ?assertEqual(M1, M2).

addValueByName_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, "Krakow", {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  ?assertEqual(M2, #{"Krakow" => {station,{100,10},[{measurement, "PM10", 100, {{2016, 2, 10}, {14, 28, 16}} }]}, "Poznan" => {station, {150, 30}, []}}).

addValueByLocation_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  ?assertEqual(M2, #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, [{measurement, "PM10", 100, {{2016, 2, 10}, {14, 28, 16}} }]}}).

addValueRedundant_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  M3 = pollution:addValue(M2, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  ?assertEqual(M2, M3).

addValueNoExistingStation_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 20}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  ?assertEqual(M2, M1).

removeValueName_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  M3 = pollution:removeValue(M2, "Poznan", {{2016, 2, 10}, {14, 28, 16}}, "PM10"),
  ?assertEqual(M3, M1).

removeValueLocation_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  M3 = pollution:removeValue(M2, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10"),
  ?assertEqual(M3, M1).

removeValueNoExistingStation_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  M3 = pollution:removeValue(M2, {150, 10}, {{2016, 2, 10}, {14, 28, 16}}, "PM10"),
  ?assertEqual(M3, M2).

removeValueNoExistingMeasurement1_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  M3 = pollution:removeValue(M2, {150, 30}, {{2016, 2, 10}, {14, 28, 10}}, "PM10"),
  ?assertEqual(M3, M2).

removeValueNoExistingMeasurement2_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  M3 = pollution:removeValue(M2, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM1"),
  ?assertEqual(M3, M2).

getValueName_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  Val = pollution:getValue(M2, "Poznan", {{2016, 2, 10}, {14, 28, 16}}, "PM10"),
  ?assertEqual(Val, 100).

getValueLocation_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  Val = pollution:getValue(M2, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10"),
  ?assertEqual(Val, 100).

getValueNoExistingLocation_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  Val = pollution:getValue(M2, {150, 20}, {{2016, 2, 10}, {14, 28, 16}}, "PM10"),
  ?assertEqual(-1, Val).

getValueEmptyMonitor_test() ->
  Val = pollution:getValue(#{}, {150, 20}, {{2016, 2, 10}, {14, 28, 16}}, "PM10"),
  ?assertEqual(-1, Val).

getStationMeanName_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  M3 = pollution:addValue(M2, {150, 30}, {{2016, 2, 11}, {14, 28, 16}}, "PM10", 150),
  M4 = pollution:addValue(M3, {150, 30}, {{2016, 2, 12}, {14, 28, 16}}, "PM10", 125),
  Val = pollution:getStationMean(M4, "Poznan", "PM10"),
  ?assert(125 == Val).

getStationMeanLocation_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  M3 = pollution:addValue(M2, {150, 30}, {{2016, 2, 11}, {14, 28, 16}}, "PM10", 150),
  M4 = pollution:addValue(M3, {150, 30}, {{2016, 2, 12}, {14, 28, 16}}, "PM10", 125),
  Val = pollution:getStationMean(M4, {150, 30}, "PM10"),
  ?assert(125 == Val).

getStationMeanNoExistingStation_test() ->
  Val = pollution:getStationMean(#{}, "K", "PM10"),
  ?assertEqual(-1, Val).

getStationMeanNoMeasurements1_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  M3 = pollution:addValue(M2, {150, 30}, {{2016, 2, 11}, {14, 28, 16}}, "PM10", 150),
  M4 = pollution:addValue(M3, {150, 30}, {{2016, 2, 12}, {14, 28, 16}}, "PM10", 125),
  Val = pollution:getStationMean(M4, "Krakow", "PM10"),
  ?assert(-1 == Val).

getStationMeanNoMeasurements2_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  M3 = pollution:addValue(M2, {150, 30}, {{2016, 2, 11}, {14, 28, 16}}, "PM10", 150),
  M4 = pollution:addValue(M3, {150, 30}, {{2016, 2, 12}, {14, 28, 16}}, "PM10", 125),
  Val = pollution:getStationMean(M4, "Poznan", "PM2"),
  ?assert(-1 == Val).

getDailyMeanNormal_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 11}, {14, 28, 16}}, "PM10", 100),
  M4 = pollution:addValue(M2, {150, 30}, {{2016, 2, 11}, {14, 28, 10}}, "PM10", 150),
  M5 = pollution:addValue(M4, {150, 30}, {{2016, 2, 12}, {14, 28, 10}}, "PM10", 200),
  Val = pollution:getDailyMean(M5, {{2016, 2, 11}, {14, 28, 10}}, "PM10"),
  ?assert(Val == 125).

getDailyMeanNoMeasurements_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  Val = pollution:getDailyMean(M1, {{2016, 2, 11}, {14, 28, 10}}, "PM10"),
  ?assert(Val == -1).

getDailyMeanNoStations_test() ->
  Val = pollution:getDailyMean(#{}, {{2016, 2, 11}, {14, 28, 10}}, "PM10"),
  ?assert(Val == -1).

getDailyAverageDataCount_test() ->
  M1 = #{"Krakow" => {station,{100,10},[]}, "Poznan" => {station, {150, 30}, []}},
  M2 = pollution:addValue(M1, {150, 30}, {{2016, 2, 10}, {14, 28, 16}}, "PM10", 100),
  M3 = pollution:addValue(M2, {150, 30}, {{2016, 2, 10}, {14, 21, 16}}, "PM2", 150),
  M4 = pollution:addValue(M3, {150, 30}, {{2016, 2, 12}, {14, 28, 16}}, "PM10", 125),

  Val = pollution:getDailyAverageDataCount(M4),
  ?assertEqual(Val, 1.5).
