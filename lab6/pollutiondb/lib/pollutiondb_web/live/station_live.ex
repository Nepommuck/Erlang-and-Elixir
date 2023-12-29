defmodule PollutiondbWeb.StationLive do
  use PollutiondbWeb, :live_view
 
  alias Pollutiondb.Station
 
  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.getAll(), name: "", lat: "", lon: "")
    {:ok, socket}
  end
 
  def render(assigns) do
    ~H"""    
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