-module(pollution).
-author("pawel").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3]).

-record(monitor, {name_map, coordinate_map}).
-record(station, {name, coordinates, readings=[]}).
-record(reading, {type, value, date_time}).


create_monitor() ->
  #monitor{name_map = maps:new(), coordinate_map = maps:new()}.


add_station(Name, Coordinates, Monitor) ->
  Is_id_unique = not maps:is_key(Name, Monitor#monitor.name_map)
    and not maps:is_key(Coordinates, Monitor#monitor.coordinate_map),

  case Is_id_unique of
    false -> error;
    true -> #monitor{
      name_map = maps:put(Name,
        #station{name = Name, coordinates = Coordinates}, Monitor#monitor.name_map),
      coordinate_map = maps:put(Coordinates,
        #station{name = Name, coordinates = Coordinates}, Monitor#monitor.coordinate_map)
    }
  end.


get_station(Station_id, Monitor) ->
  Station = case Station_id of
              [_ | _] -> maps:get(Station_id, Monitor#monitor.name_map);
              {_, _} -> maps:get(Station_id, Monitor#monitor.coordinate_map);
              _ -> none
            end,
  Station.


get_station_name_and_coordinates(Station_id, Monitor) ->
  case get_station(Station_id, Monitor) of
    none -> none;
    Station -> {Station#station.name, Station#station.coordinates}
  end.


update_station_readings(Station_id, New_readings, Monitor) ->
  Station = get_station(Station_id, Monitor),
  {Station_name, Station_coordinates} = get_station_name_and_coordinates(Station_id, Monitor),
  #monitor{
    name_map = maps:put(
      Station_name, Station#station{readings = New_readings}, Monitor#monitor.name_map),
    coordinate_map = maps:put(
      Station_coordinates, Station#station{readings = New_readings}, Monitor#monitor.coordinate_map)
  }.


add_value(Station_id, Date_time, Reading_type, Reading_value, Monitor) ->
  New_reading = #reading{type = Reading_type, value = Reading_value, date_time = Date_time},
  Station = get_station(Station_id, Monitor),
  Updated_readings_list = Station#station.readings ++ [New_reading],

  update_station_readings(Station_id, Updated_readings_list, Monitor).


remove_value(Station_id, Reading_date_time, Reading_type, Monitor) ->
  Station = get_station(Station_id, Monitor),
  Updated_readings_list = lists:filter(
    fun(R) -> (R#reading.type /= Reading_type) or (R#reading.date_time /= Reading_date_time) end,
    Station#station.readings
  ),
  update_station_readings(Station_id, Updated_readings_list, Monitor).


get_one_value(Station_id, Reading_type, Reading_date_time, Monitor) ->
  Station = get_station(Station_id, Monitor),
  Station_readings_list = Station#station.readings,
  [Reading | _] = lists:filter(
    fun(R) -> (R#reading.type == Reading_type) and (R#reading.date_time == Reading_date_time) end,
    Station_readings_list
  ),
  Reading#reading.value.


get_station_mean(Station_id, Reading_type, Monitor) ->
  Station = get_station(Station_id, Monitor),
  Readings_of_type = lists:filter(
    fun(R) -> (R#reading.type == Reading_type) end,
    Station#station.readings
  ),
  case length(Readings_of_type) of
    0 -> none;
    Length -> lists:foldl(
      fun(R, Sum) -> Sum + R#reading.value end, 0, Readings_of_type
    ) / Length
  end.


get_daily_mean(Reading_type, Date, Monitor) ->
  Get_all_readings = fun
                       F([]) -> [];
                       F([Station | Tail]) -> Station#station.readings ++ F(Tail)
                     end,
  DateTime_Into_Date = fun({Date_, _T}) -> Date_ end,

  Day_readings = lists:filter(
    fun(R) -> (R#reading.type == Reading_type) and (DateTime_Into_Date(R#reading.date_time) == Date) end,
    Get_all_readings(maps:values(Monitor#monitor.name_map))
  ),
  case length(Day_readings) of
    0 -> none;
    Length -> lists:foldl(
      fun(R, Sum) -> Sum + R#reading.value end, 0, Day_readings
    ) / Length
  end.


%%%% DATA:
%%P = pollution:create_monitor().
%%P1 = pollution:add_station("Aleja Slowackiego", {50.0730, 19.9327}, P).
%%P2 = pollution:add_station("Zakopianka", {50.0062, 19.9244}, P1).
%%{Day, _} = calendar:local_time().
%%P3 = pollution:add_value({50.0730, 19.9327}, calendar:local_time(), "PM10", 59, P2).
%%P4 = pollution:add_value("Aleja Slowackiego", calendar:local_time(), "PM2,5", 113, P3).
%%P5 = pollution:add_value({50.0730, 19.9327}, {Day, {23,17,1}}, "PM10", 70, P4).
%%P6 = pollution:add_value("Aleja Slowackiego", {{2005,4,2},{21,37,00}}, "PM2,5", 420, P5).
%%P7 = pollution:add_value("Zakopianka", calendar:local_time(), "PM10", 111, P6).
%%
%%%% TESTS:
%%pollution:get_station_mean("Aleja Slowackiego", "PM2,5", P7).
%%%% Output: 266.5
%%
%%P8 = pollution:remove_value("Aleja Slowackiego", {{2005,4,2},{21,37,00}}, "PM2,5", P7).
%%pollution:get_station_mean("Aleja Slowackiego", "PM2,5", P8).
%%%% Output: 113.0
%%
%%pollution:get_daily_mean("PM10", Day, P8).
%%%% Output: 80.0
