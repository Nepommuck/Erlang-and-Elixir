defmodule Pollutiondb.Parser do
  defp toFloat(int) do int / 1 end

  def readStationsFromFile(readPartial \\ false) do
    data = readDataFromFile(readPartial)
    identifyStations(data)
    |> Enum.map(
      fn {{longitude, latitude}, index} -> 
        Pollutiondb.Station.createNewStation(
          "Station " <> (index+1 |> Integer.to_string()),
          longitude, latitude
        )
      end)

    parseReadings(data)
    |> Enum.map(fn reading -> 
      Pollutiondb.Reading.add(reading) end)
  end
  
  defp parseLine(raw_line) do
    [date, time, longitude, latitude, pollution_level] = String.split(raw_line, ",")

    datetime = DateTime.new!(date |> parseDate, time |> parseTime)
    lon = longitude |> String.to_float()
    lat = latitude |> String.to_float()
    pollutionLevel = pollution_level |> String.to_integer() |> toFloat()

    {datetime, {lon, lat}, pollutionLevel}
  end

  defp identifyStations(data) do
    data
    |> Enum.map(
      fn {_datetime, coordinates, _pollutionLevel} -> 
        coordinates end)
    |> Enum.uniq_by(fn x -> x end)
    |> Enum.with_index()
  end

  defp parseReadings(data) do
    data
    |> Enum.map(
      fn {datetime, {longitude, latitude}, pollutionLevel} -> 
        %Pollutiondb.Reading {
          datetime: datetime, type: "PM10", value: pollutionLevel,
          station: Pollutiondb.Station.findByLocation(longitude, latitude)
          |> List.first()
        }
      end)    
  end

  defp parseDate(date) do
    {day, month, year} = date 
    |> String.split("-")
    |> Enum.map(fn x -> x |> String.to_integer() end)
    |> List.to_tuple()

    Date.new!(year, month, day)
  end

  defp parseTime(time) do
    {hour, minutes, seconds} = time <> ":00" 
    |> String.split(":")
    |> Enum.map(fn x -> x |> String.to_integer() end)
    |> Enum.map(fn n -> min(n, 59) end)
    |> List.to_tuple()

    Time.new!(hour, minutes, seconds)
  end

  defp readDataFromFile(false) do
    readDataFromPath("../data/pollution.csv") end
  defp readDataFromFile(true) do
    readDataFromPath("../data/pollution_partial.csv") end
  
  defp readDataFromPath(path) do
    path
    |> File.read!()
    |> String.split("\r\n")
    |> Enum.map(fn line -> parseLine(line) end)
  end
  
end