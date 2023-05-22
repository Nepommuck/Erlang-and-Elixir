-module(pollution_value_collector_gen_statem).
-author("pawel").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0]).

-export([set_station/1, add_value/2, add_value/3, store_data/0]).

-define(SERVER, ?MODULE).
-define(INITIAL_VALUE, none).

%%-record(pollution_value_collector_gen_statem_state, {}).
-record(state, {monitor, station_id=none, new_readings=[]}).
-record(reading, {type, value, date_time}).


print_error(Error_message) ->
  io:format("ERROR: ~w. ~n", [Error_message]),
  ok.


start_link() ->
  Monitor = pollution:create_monitor(),
  gen_statem:start_link({local, ?SERVER}, ?MODULE, Monitor, []).


init(Monitor) ->
  {ok, station_not_set, #state{monitor = Monitor}}.


%% User API
set_station(Station_id) ->
  gen_statem:cast(?SERVER, {set_station, Station_id}).

add_value(Reading_type, Reading_value) ->
  add_value(Reading_type, Reading_value, calendar:local_time()).
add_value(Reading_type, Reading_value, Date_time) ->
  gen_statem:cast(?SERVER, {add_value, Reading_type, Reading_value, Date_time}).

store_data() ->
  gen_statem:cast(?SERVER, {store_data}).


station_not_set(_EventType, {set_station, Station_id}, State = #state) ->
  case pollution:get_station(Station_id, State#state.monitor) of
    {error, Error_message} ->
      print_error(Error_message),
      {keep_state_and_data};
    _Station ->
      {next_state, station_set, State#state{station_id = Station_id}}
  end.


station_set(_EventType, {add_value, Reading_type, Reading_value, Date_time}, State = #state) ->
  New_reading = #reading{type = Reading_type, value = Reading_value, date_time = Date_time},
  Updated_reading_list = [New_reading | State#state.new_readings],
  {keep_state, State#state{new_readings = Updated_reading_list}};

station_set(_EventType, {store_data}, State = #state) ->
  commit_data(State#state.new_readings, State#state.station_id, State#state.monitor),
  {next_state, station_not_set, #state{monitor = State#state.monitor}}.


commit_data([], _Station_id, _Monitor) ->
  ok;
commit_data([Reading = #reading | Tail], Station_id, Monitor) ->
  [Date_time, Reading_type, Reading_value] =
    [Reading#reading.date_time, Reading#reading.type, Reading#reading.value],

  case pollution:add_value(Station_id, Date_time, Reading_type, Reading_value, Monitor) of
    {error, Error_message} ->
      print_error(Error_message),
      commit_data(Tail, Station_id, Monitor);
    New_monitor ->
      commit_data(Tail, Station_id, New_monitor)
  end.


terminate(Reason, StateName, State) ->
  io:format("ERROR: ~w. ~n", [Reason]),
  io:format("In state: ~w. ~n", [StateName]),
  io:format("With data stored: ~w. ~n", [State]),
  ok.


callback_mode() ->
  handle_event_function.
