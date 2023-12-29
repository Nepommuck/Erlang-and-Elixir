defmodule Pollutiondb.Reading do
  use Ecto.Schema
  require Ecto.Query
 
  schema "readings" do
      field :datetime, :utc_datetime
      field :type, :string
      field :value, :float
      
      belongs_to :station, Pollutiondb.Station
  end

  def insertMockData() do
    [
      {"Prądnik Biały", "PM10", 121.5},
      {"Prądnik Biały", "PM2.5", 63.5},
      {"Krowodrza Górka", "PM10", 21.5},
      {"Nowa Huta", "PM10", 28.5},
    ]
    |> Enum.map(fn {station_name, type, val} -> 
      addNow(station_name |> Pollutiondb.Station.findByName(), type, val) end)

    [
      {"Krowodrza Górka", "PM10", 17.5}
    ]
    |> Enum.map(fn {station_name, type, val} -> 
      add(%Pollutiondb.Reading{
         station: station_name |> Pollutiondb.Station.findByName(), 
         type: type, value: val,
         datetime: ~U[2023-06-20 23:11:50Z]
       }) end)
  end
  
  def getAll() do
    Pollutiondb.Repo.all(Pollutiondb.Reading)
    |> Pollutiondb.Repo.preload(:station)
  end

  def add(reading) do
    Pollutiondb.Repo.insert(reading)
  end
  
  def addNow(station, type, value) do
    add(%Pollutiondb.Reading{
         station: station, type: type, value: value,
         datetime: DateTime.utc_now |> DateTime.truncate(:second)
         # datetime: ~U[2023-06-20 23:11:50Z]
       })
  end

  def findByDate(date) do
    minDateTime = DateTime.new!(date, ~T[00:00:00])
    maxDateTime = DateTime.add(minDateTime, 24*60*60, :second)
    Ecto.Query.from(
      r in Pollutiondb.Reading,
      where: ^minDateTime <= r.datetime,
      where: r.datetime <= ^maxDateTime
    )
    |> Pollutiondb.Repo.all
  end
  
end