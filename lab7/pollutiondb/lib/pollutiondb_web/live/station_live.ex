defmodule PollutiondbWeb.StationLive do
  use PollutiondbWeb, :live_view
 
  alias Pollutiondb.Station
 
  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.getAll(), name: "", lat: "", lon: "")
    {:ok, socket}
  end

  def to_float(string, default) do
    case Float.parse(string) do
      :error -> default
      {result, _remainder} -> result
    end
  end

  def handle_event("insert", %{"name" => name, "lat" => lat, "lon" => lon}, socket) do
    Station.add(%Station{name: name, lat: to_float(lat, 0.0), lon: to_float(lon, 0.0)})
    socket = assign(socket, stations: Station.getAll(), name: name, lat: lat, lon: lon)
    {:noreply, socket}
  end

  # def handle_event("update", %{"query" => query}, socket) do
  #   # socket = assign(socket, stations: Station.getAll(), name: name, lat: lat, lon: lon)
  #   # {:noreply, socket}
  #   :ok
  # end

  # def handle_event("append", %{"query" => query}, socket) do
  #   # Station.add(%Station{name: name, lat: to_float(lat, 0.0), lon: to_float(lon, 0.0)})
  #   # socket = assign(socket, stations: Station.getAll(), name: name, lat: lat, lon: lon)
  #   # {:noreply, socket}
  #   :ok
  # end
 
  def render(assigns) do
    # Find station
    # <form phx-change="update">
    #   Name: <input type="text" name="query" value={@query} /><br/>
    #   <input type="submit" /> 
    # </form>
    
    ~H"""    
    Create new station
    <form phx-submit="insert">
      Name: <input type="text" name="name" value={@name} /><br/>
      Lat: <input type="number" name="lat" value={@lat} /><br/>
      Lon: <input type="number" name="lon" value={@lon} /><br/>
      <input type="submit" /> 
    </form>

    <table>
      <tr>
        <th>Name</th><th>Longitude</th><th>Latitude</th>
      </tr>
      <%= for station <- @stations do %>
        <tr>
          <td><%= station.name %></td>
          <td><%= station.lon %></td>
          <td><%= station.lat %></td>
        </tr>
      <% end %>
    </table>
    """
  end
end