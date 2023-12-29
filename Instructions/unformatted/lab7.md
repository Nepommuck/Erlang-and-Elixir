Laboratorium nr 7 - Elixir Phoenix



Przed zajęciami proszę o:

    Nie używamy replita z poprzednich ćwiczeń!
    utworzenie konta w serwisie replit: https://replit.com/ (jeśli jeszcze nie mamy)
    utworzenie w nim nowego Repla dla języka Elixir, nazwanie go w formacie „Imię Nazwisko Elixir lab7”
    przetestowanie jego działania
    udostępnienie Repla przez „Private join link” i wpisanie linku do niego w https://forms.gle/rKNq44eSyMDCqhP8A


Cele ćwiczenia

    Zapoznanie się z frameworkiem Phoenix
    Poznanie mechanizmu LiveView
    Zbudowanie aplikacji webowej w oparciu o ww. technologie


Przebieg zajęć
Konfiguracja środowiska


    Tworzymy nowego replita, z zainstalowanym Elixirem (ale nie Phoenixem).
    W pliku replit.nix (jest to plik ukryty) dodajemy:

    pkgs.sqlite
    pkgs.inotify-tools

    W konsoli wpisujemy

    kill 1

    W wyniku uruchomienia tych poleceń środowisko jest modyfikowane i sesja jest resetowana.
    Instalujemy hex-a

    mix local.hex

    Instalujemy Phoenix-a w wersji 1.6.16:

    mix archive.install hex phx_new 1.6.16

Utworzenie i konfiguracja aplikacji


    Tworzymy nową aplikację pollutiondb, wyłączając automatyczne dodanie zależności do Ecto:

    mix phx.new pollutiondb --no-ecto

    W katalogu pollutiondb/config w pliku dev.exs zmieniamy linijkę:

    http: [ip: {127, 0, 0, 1}, port: 4000],

    na

    http: [ip: {0, 0, 0, 0}, port: 4000],

    W katalogu pollutiondb/ w pliku mix.exs w sekcji dependencies dodajemy

    {:ecto_sql, "~> 3.0"},
    {:ecto_sqlite3, "~> 0.10"},

    Ściągamy zależność za pomocą mix-a (w katalogu pollutiondb):

    mix deps.get

    W katalogu pollutiondb/config w pliku dev.exs dodajemy linijki:

    config :pollutiondb, ecto_repos: [Pollutiondb.Repo]
    config :pollutiondb, Pollutiondb.Repo, database: "database/pollutiondb.db"

    Tworzymy katalog pollutiondb/database i kopiujemy do niego 3 pliki pollutiondb.db[-shm,-wal] z poprzedniej wersji aplikacji.
    Tworzymy katalog pollutiondb/priv/repo/migrations i kopiujemy do niego 2 pliki z poprzedniej aplikacji.
    W katalogu pollutiondb/lib/pollutiondb/ w pliku application.ex w sekcji children dodajemy:

    Pollutiondb.Repo,

    Do tego katalogu kopiujemy pliki reading.ex, repo.ex oraz station.ex z poprzedniej aplikacji.
    Uruchamiamy serwer Phoenix-a upewniając się, że nie ma błędów:

    mix phx.server

    W wyniku wywołania tego polecenia, otworzy się okienko webview. Kopiujemy adres naszej aplikacji i otwieramy ją w osobnej zakładce przeglądarki. Okno webview można zamknąć, ponieważ będzie ono generować niepotrzebnie błędy.
    Jeśli w wyniku wywołania polecenia mix phx.server otrzymujemy komunikat

    Application pollutiondb exited: shutdown

    usuwamy pliki z katalogu polutiondb/databse i w konsoli wywołujemy polecenia

    mix ecto.create
    mix ecto.migrate

    W wyniku wywołania tych poleceń usunięte jednak zostaną dane w naszej bazie danych.

Tworzenie i wyświetlanie stacji


    Tworzymy katalog pollutiondb/lib/pollutiondb_web/live
    Tworzymy w katalogu pollutiondb/lib/pollutiondb_web/live plik station_live.ex:

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

    W pliku pollutiondb/lib/pollutiondb_web/router.ex zmieniamy linijkę:

      get "/", PageController, :index

    na

      live "/", StationLive

    Sprawdzamy co dzieje się w zakładce, w której uruchomiona jest aplikacja. Jeśli w bazie danych były jakieś stacje, to powinny one wyświetlać się w głównym widoku aplikacji.
    Zapoznajemy się z dokumentacją mechanizmu LiveView.
    W szablonie (metoda render) przed tabelką ze stacjami dodaj formularz służący do tworzenia nowych stacji:

    Create new station
    <form phx-submit="insert">
      Name: <input type="text" name="name" value={@name} /><br/>
      Lat: <input type="number" name="lat" value={@lat} /><br/>
      Lon: <input type="number" name="lon" value={@lon} /><br/>
      <input type="submit" /> 
    </form>

    Dodaj również metodę obsługi dodawania stacji:

      def handle_event("insert", %{"name" => name, "lat" => lat, "lon" => lon}, socket) do
        Station.add(%Station{name: name, lat: to_float(lat, 0.0), lon: to_float(lon, 0.0)})
        socket = assign(socket, stations: Station.getAll(), name: name, lat: lat, lon: lon)
        {:noreply, socket}
      end

    W powyższym kodzie wykorzystywana jest metoda to_float. Należy ją samemu zaimplementować w taki sposób, aby w przypadku błędu związanego z parsowaniem przekazanej wartości, zwracana była druga wartość (wartość domyślna).
    Zweryfikuj poprawność działania formularza, dodając kilka nowych stacji do bazy.
    W szablonie dodaj osobny formularz do wyszukiwania stacji według ich nazwy. Mechanizm ten powinien działać dla zdarzenia phx-change, a nie phx-submit, jak w przypadku poprzedniej akcji. Formularz powinien posiadać jedno pole tekstowe o nazwie query.
    Dodaj akcję obsługującą zderzenie zmiany formularza. Jeśli wartość zapytania jest pusta, to powinny być wyświetlane wszystkie stacje. Jeśli jest niepusta, to powinna być wyświetlana stacja o wskazanej nazwie. Wykorzystaj kod w module Station, który był napisany na poprzednich zajęciach.

Wyszukiwanie stacji według pozycji


    Utwórz nowy moduł PollutiondbWeb.StationRangeLive, który również znajduje się w katalogu live.
    W pliku router.ex dodaj linijkę:

    live "/range", StationRangeLive

    Dodaj metodę render, która poza tabelką wyświetlającą stacje posiada następujący formularz (wartości minimalne i maksymalne zmień tak, aby pasowały do danych zgromadzonych w bazie):

    <form phx-change="update">
    Lat min
    <input type="range" min="0" max="5" name="lat_min" value={@lat_min}/><br/>
    Lat max 
    <input type="range" min="0" max="5" name="lat_max" value={@lat_max}/><br/>
    Lon min
    <input type="range" min="0" max="5" name="lon_min" value={@lon_min}/><br/>
    Lon max
    <input type="range" min="0" max="5" name="lon_max" value={@lon_max}/><br/>
    </form>

    Zdefiniuj metodę mount, która poza ładowaniem wszystkich stacji (pole stations), definiuje domyślne wartości min_lat, max_lat, min_lon oraz max_lon. Bez tych wartości widok nie będzie działał.
    Sprawdź działanie nowego modułu dodając w ścieżce aplikacji /range np. https://pm10-db-web-2.apohllo.repl.co/range
    Dodaj metodę handle_event, która będzie aktualizowała listę wyświetlanych stacji, zgodnie z wartościami modyfikowanych parametrów. Pamiętaj aby skonwertować dane tekstowe na wartości liczbowe.
    Manualnie przetestuj działanie nowego modułu. Upewnij się, że po zmianie kilku parametrów pozostałe parametry nie są resetowane do wartości domyślnych.

Wyświetlanie odczytów ze stacji


    Utwórz nowy moduł PollutiondbWeb.ReadingLive, który również znajduje się w katalogu live.
    Dodaj ścieżkę /readings która wywołuje ten moduł.
    Zmodyfikuj moduł Pollutiondb.Reading tak, aby zawierał metodę zwracającą 10 ostatnich odczytów, w kolejności od najnowszego. Możesz wykorzystać ten kod:

    Ecto.Query.from(r in Pollutiondb.Reading,
      limit: 10, order_by: [desc: r.date, desc: r.time])
      |> Pollutiondb.Repo.all()
      |> Pollutiondb.Repo.preload(:station)

    W metodzie render wyświetl odczyty z bazy danych ograniczając je do 10 najnowszych wyników. W widoku powinna pojawiać się nazwa stacji, data i godzina odczytu oraz typ i wartość odczytu.
    W module Pollutiondb.Reading dodaj metodę pozwalającą na odczyt 10 najnowszych wyników z określonego dnia. Alternatywnie Możesz zmodyfikować istniejącą metodę getByDate, tak by zwracała 10 najnowszych wyników.
    Dodaj formularz pozwalający na wyszukiwanie 10 najnowszych odczytów z określonej daty. W formularzu powinno być jedno pole pozwalające na wybór daty. Pamiętaj aby w metodzie mount ustawić wartość date, która będzie wykorzystywana w polu formularza na odpowiednią wartość domyślną - dzień bieżący.
    Dodaj metodę handle_event, która obsłuży wywołanie związane ze zmianą wartości pola date w formularzu. Jeśli wartość daty będzie pusta, to wyświetl wartość domyślną, czyli 10 najnowszych wyników.
    Kod konwertujący tekst na datę umieść w metodzie prywatnej to_date. Jeśli parsowanie daty nie powiedzie się, to wyświetl wyniki z dnia bieżącego.
    Manualnie przetestuj działanie mechanizmu wyszukiwania według daty.

Tworzenie nowych odczytów


    W tym samym module w szablonie dodaj formularz pozwalający na dodawanie nowych odczytów. Formularz ten powinien zawierać listę rozwijaną wszystkich stacji zgromadzonych w bazie. Konieczne jest ustawienie zmiennej stations w metodzie mount oraz wyświetlenie stacji w szablonie. Możesz wykorzystać następujący kod HTML do obsługi listy rozwijanej:

    <select name="station_id">
      <%= for station <- @stations do %>
        <option label={station.name} value={station.id} selected={station.id == @station_id}/>
      <% end %>
    </select>

    Dodaj obsługę mechanizmu tworzącego nowe odczyty. Pamiętaj aby skonwertować wartość odczytu na odpowiedni typ wartości kompatybilny z typem tego pola w bazie danych. Dotyczy to również identyfikatora stacji, która została wybrana na liście rozwijalnej. Datę i czas ustaw według wartości bieżących. Możesz wykorzystać do tego istniejącą metodę w module Pollutiondb.Reading.
    Metoda insert wymaga jednak, aby przekazać do niej strukturę Pollutiondb.Station. Możesz utworzyć namiastkę tej struktury, ustalając tylko jej identyfikator:

    %Station{id: to_int(station_id, 1)}

    Metoda to_int konwertuje tekst na wartość liczbową.
    Pamiętaj o tym, aby w metodzie mount oraz metodzie handle_event dodać odpowiednie pola, które będą ustawiały wartości domyślne w formularzu tworzenia odczytów (type, value, station_id). Ułatwi to korzystanie z formularza.
    Przetestuj manualnie działanie utworzonego mechanizmu.

Zadanie domowe

    ostatnie zajęcia - już nie ma zadania !

