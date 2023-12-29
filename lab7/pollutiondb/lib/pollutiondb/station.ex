defmodule Pollutiondb.Station do
  use Ecto.Schema
  require Ecto.Query
 
  schema "stations" do
      field :name, :string
      field :lon, :float
      field :lat, :float
      
      has_many :readings, Pollutiondb.Reading
  end
  
  def insertMockData() do
    [
       %Pollutiondb.Station{
         name: "Aleja Słowackiego", lon: 1.1, lat: 1.1,
       },
       %Pollutiondb.Station{
         name: "Nowa Huta", lon: 2.1, lat: 2.1,
       },
       %Pollutiondb.Station{name: "Krowodrza Górka", lon: 3.1, lat: 3.1},
       %Pollutiondb.Station{name: "Prądnik Biały", lon: 4.1, lat: 4.1}
    ] 
    |> Enum.map(fn station -> station |> add() end)
    
    :ok
  end

  defp validate(station, changesmap) do
    station
    |> Ecto.Changeset.cast(changesmap, [:name, :lon, :lat])
    |> Ecto.Changeset.validate_required([:name, :lon, :lat])
    |> Ecto.Changeset.validate_number(
      :lon, greater_than: -200.0, less_than: 200)
    |> Ecto.Changeset.validate_number(
      :lat, greater_than: -100.0, less_than: 100)
  end

  def add(station) do
    Pollutiondb.Repo.insert(station)
  end
  
  def createNewStation(name, lon, lat) do
    %Pollutiondb.Station{}
    |> validate(%{name: name, lon: lon, lat: lat})
    |> Pollutiondb.Repo.insert()
  end
  
  def getAll() do
    Pollutiondb.Repo.all(Pollutiondb.Station)
    |> Pollutiondb.Repo.preload(:readings)
  end
  
  def getById(id) do
    Pollutiondb.Repo.get(Pollutiondb.Station, id)
  end
  
  def remove(station) do
    Pollutiondb.Repo.delete(station)
  end

  def clearDatabase() do
    getAll()
    |> Enum.map(fn(station) -> remove(station) end)
    :ok
  end
  
  def findByName(name) do
    Ecto.Query.where(Pollutiondb.Station, name: ^name) 
    |> Pollutiondb.Repo.all()
    |> Pollutiondb.Repo.preload(:readings)
    |> List.first()
  end
  
  def findByLocation(lon, lat) do
    Ecto.Query.from(
      s in Pollutiondb.Station, 
      where: s.lon == ^lon,
      where: s.lat == ^lat
    )
    |> Pollutiondb.Repo.all
    |> Pollutiondb.Repo.preload(:readings)
  end
  
  def findByLocationRange(lonMin, lonMax, latMin, latMax) do
    Ecto.Query.from(
      s in Pollutiondb.Station, 
      where: ^lonMin <= s.lon and s.lon <= ^lonMax,
      where: ^latMin <= s.lat and s.lat <= ^latMax
    )
    |> Pollutiondb.Repo.all
  end

  def updateName(station, newname) do
    station
    |> validate(%{name: newname})
    |> Pollutiondb.Repo.update
  end
end
