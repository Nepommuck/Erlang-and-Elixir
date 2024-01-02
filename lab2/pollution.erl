-module(pollution).
-author("pawel").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3,
  get_daily_mean/3, get_correlation/4]).


-record(monitor, {name_map, coordinate_map}).
-record(station, {name, coordinates, readings=[]}).
-record(reading, {type, value, date_time}).

%% Monitor: Record(
%%     name_map: Map(Key: String, Value: Station),
%%     coordinate_map: Map(Key: {Float, Float}, Value: Station)
%% )
%% Station: Record(
%%     name: String, coordinates: {Float, Float}, readings: [Reading]
%% )
%% Reading: Record(
%%     type: String, value: Float, date_time: {{Int, Int, Int}, {Int, Int, Int}}
%% )


%% "Private":
get_station(Station_id, Monitor) ->
  Station = case Station_id of
              [_ | _] -> case maps:is_key(Station_id, Monitor#monitor.name_map) of
                           true -> maps:get(Station_id, Monitor#monitor.name_map);
                           false -> {error, no_such_station_name}
                         end;
              {_, _} -> case maps:is_key(Station_id, Monitor#monitor.coordinate_map) of
                          true -> maps:get(Station_id, Monitor#monitor.coordinate_map);
                          false -> {error, no_such_coordinates}
                        end
            end,
  Station.


get_station_name_and_coordinates(Station_id, Monitor) ->
  case get_station(Station_id, Monitor) of
    {error, _} -> {error, station_not_found};
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


get_standard_deviation(Station, Reading_type) ->
  Get_delta = fun
                F([]) -> [];
                F([_]) -> [];
                F([El1, El2 | Tail]) -> [El2 - El1 | F([El2 | Tail])]
              end,
  Readings_of_type = lists:filter(fun(R) -> (R#reading.type == Reading_type) end, Station#station.readings),
  Deltas = Get_delta(lists:map(fun(R) -> R#reading.value end, Readings_of_type)),

  case length(Deltas) of
    0 -> {error, no_such_readings};
    1 -> 0;
    Length ->
      Mean = lists:sum(Deltas) / Length,
      math:sqrt(
        lists:sum([math:pow(X - Mean, 2) || X <- Deltas]) / Length
      )
  end.


%% "Public":
create_monitor() ->
  #monitor{name_map = maps:new(), coordinate_map = maps:new()}.


add_station(Name, Coordinates, Monitor) ->
  Is_id_unique = not maps:is_key(Name, Monitor#monitor.name_map)
    and not maps:is_key(Coordinates, Monitor#monitor.coordinate_map),

  case Is_id_unique of
    false -> {error, station_is_not_unique};
    true -> #monitor{
      name_map = maps:put(Name,
        #station{name = Name, coordinates = Coordinates}, Monitor#monitor.name_map),
      coordinate_map = maps:put(Coordinates,
        #station{name = Name, coordinates = Coordinates}, Monitor#monitor.coordinate_map)
    }
  end.


add_value(Station_id, Date_time, Reading_type, Reading_value, Monitor) ->
  New_reading = #reading{type = Reading_type, value = Reading_value, date_time = Date_time},
  Station = get_station(Station_id, Monitor),

  case Station of
    {error, _} -> {error, no_station_with_such_id};
    _ ->
      Updated_readings_list = Station#station.readings ++ [New_reading],
      case get_one_value(Station_id, Date_time, Reading_type, Monitor) of
        {error, _} -> update_station_readings(Station_id, Updated_readings_list, Monitor);
        _ -> {error, such_value_already_exists}
      end
  end.


remove_value(Station_id, Reading_date_time, Reading_type, Monitor) ->
  Station = get_station(Station_id, Monitor),

  case Station of
    {error, _} -> {error, no_station_with_such_id};
    _ ->
      Updated_readings_list = lists:filter(
        fun(R) -> (R#reading.type /= Reading_type) or (R#reading.date_time /= Reading_date_time) end,
        Station#station.readings
      ),
      case get_one_value(Station_id, Reading_date_time, Reading_type, Monitor) of
        {error, _} -> {error, value_doesnt_exist};
        _ -> update_station_readings(Station_id, Updated_readings_list, Monitor)
      end
  end.


get_one_value(Station_id, Reading_date_time, Reading_type, Monitor) ->
  Station = get_station(Station_id, Monitor),
  case Station of
    {error, _} -> {error, no_such_station};
    _ ->
      Station_readings_list = Station#station.readings,
      Readings_matching = lists:filter(
        fun(R) -> (R#reading.type == Reading_type) and (R#reading.date_time == Reading_date_time) end,
        Station_readings_list
      ),
      case length(Readings_matching) of
        0 -> {error, no_such_value};
        1 ->
          [Reading] = Readings_matching,
          Reading#reading.value;
        _ -> {error, multiple_values_found}
      end
  end.


get_station_mean(Station_id, Reading_type, Monitor) ->
  Station = get_station(Station_id, Monitor),
  case Station of
    {error, _} -> {error, no_such_station};
    _ ->
      Readings_of_type = lists:filter(
        fun(R) -> (R#reading.type == Reading_type) end,
        Station#station.readings
      ),
      case length(Readings_of_type) of
        0 -> {error, no_such_readings};
        Length -> lists:foldl(
          fun(R, Sum) -> Sum + R#reading.value end, 0, Readings_of_type
        ) / Length
      end
  end.


get_daily_mean(Reading_type, Date, Monitor) ->
  Get_all_readings = fun
                       F([]) -> [];
                       F([Station | Tail]) -> Station#station.readings ++ F(Tail)
                     end,
  Fun_DateTime_Into_Date = fun({Date_, _T}) -> Date_ end,

  Day_readings = lists:filter(
    fun(R) -> (R#reading.type == Reading_type) and (Fun_DateTime_Into_Date(R#reading.date_time) == Date) end,
    Get_all_readings(maps:values(Monitor#monitor.name_map))
  ),
  case length(Day_readings) of
    0 -> {error, no_such_readings_this_day};
    Length -> lists:foldl(
      fun(R, Sum) -> Sum + R#reading.value end, 0, Day_readings
    ) / Length
  end.


get_correlation(Station_id, Reading_type1, Reading_type2, Monitor) ->
  Station = get_station(Station_id, Monitor),
  {
    get_standard_deviation(Station, Reading_type1),
    get_standard_deviation(Station, Reading_type2)
  }.


%% Run tests:
%%
%% $ cd ./src/lab2
%% $ erl
%% $ c(pollution).
%% $ c(pollution_test).
%% $ eunit:test([pollution_test]).
