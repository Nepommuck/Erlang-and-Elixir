# Laboratorium nr 6 - Aplikacja bazodanowa Elixir Ecto

## Przed zajęciami proszę o:
1. utworzenie konta w serwisie `replit`: https://replit.com/
2. utworzenie w nim nowego Repla dla języka Elixir, nazwanie go w formacie `„Imię Nazwisko Elixir lab6”`
3. przetestowanie jego działania
4. udostępnienie Repla przez `Private join link` i wpisanie linku do niego w https://forms.gle/...


## Cele ćwiczenia
1. Zapoznanie z działaniem `mix`,
2. Poznanie założeń `Ecto`,
3. Zbudowanie aplikacji bazodanowej w Elixirze.

## Przebieg zajęć

### Utworzenie aplikacji
1. wejdź do zakładki `Shell`, wpisz
    ```
    mix new pollutiondb --sup
    ```
2. przejdź do katalogu `pollutiondb` i w nim wywołaj `mix compile`
3. możesz teraz wystartować `iex -S mix`, a w nim `:observer.start()`

### Zależności aplikacji
1. w pliku `mix.esx` zmień funkcję deps by zwracała listę zawierającą:
    ```
    {:ecto_sql, "~> 3.0"},
    {:ecto_sqlite3, "~> 0.10"}
    ```
2. w konsoli wpisz `mix deps.get`, musisz wyrazić zgodę na instalację Hexa
3. w konsoli wpisz `mix deps.compile`, musisz zgodzić się na pobranie Rebara.Kompilacja zależności chwilę potrwa...
4. w konsoli wpisz `mix compile`

### Elementy bazy danych
1. utwórz moduł `Pollutiondb.Repo`, który będzie reprezentował połączenie z bazą danych: wywołaj w konsoli `mix ecto.gen.repo -r Pollutiondb.Repo`
2. dodaj wygenerowany moduł do nadzorowanych procesów domyślnego supervisora, zgodnie z sugestią generatora
3. zmodyfikuj konfigurację połączenia, by korzystała z odpowiedniej biblioteki: w pliku `lib/pollutiondb/repo.ex` zmień adapter z `Ecto.Adapters.Postgres` na `Ecto.Adapters.SQLite3`
4. zmodyfikuj konfigurację połączenia z bazą danych, która została wygenerowana w pliku `config/config.exs`
    ```
    import Config
        
        config :pollutiondb, ecto_repos: [Pollutiondb.Repo]  
        config :pollutiondb, Pollutiondb.Repo, database: "database/pollutiondb.db"
    ```
5. utwórz pustą bazę danych: wpisz w shellu `mix ecto.create`

### Model w bazie i w aplikacji
1. każdą zmianę struktury bazy Ecto nazywa migracją. Wygeneruj pierwszą migrację, wpisując w shell polecenie `mix ecto.gen.migration create_stations`
2. w wygenerowanej funkcji change w pliku `priv/repo/migrations/datenumber_create_stations.exs` zdefiniuj strukturę pierwszej tabeli:
    ```
    create table(:stations) do
        add :name, :string
        add :lon, :float
        add :lat, :float
    end
    ```
3. utwórz tabelę w bazie wpisując w shell `mix ecto.migrate`
4. utwórz nowy moduł w pliku `lib/pollutiondb/station.ex`:
    ```
    defmodule Pollutiondb.Station do
        use Ecto.Schema
        
        schema "stations" do
            field :name, :string
            field :lon, :float
            field :lat, :float
        end
    end
    ```
5. jeśli wszystko ma działać, schematy tabel i odpowiadających im struktur muszą być zgodne; więcej szczegółów: https://hexdocs.pm/ecto/Ecto.Schema.html

### Podstawowe operacje bazodanowe
1. skompiluj aplikację (`mix compile`) i uruchom ją (`iex -S mix`)
2. dodaj pierwszy wpis (i ew kolejne) w bazie:
    ```
    station = %Pollutiondb.Station{name: "Station #1", lon: 123.4, lat: 246.8} 
    Pollutiondb.Repo.insert(station)
    ```
3. sprawdź czy coś się zapisało:
    ```
    Pollutiondb.Repo.all(Pollutiondb.Station)
    ```
4. dodaj w module Pollutiondb.Station funkcję `add(station)` i użyj jej w aplikacji by wygenerować stacje:
    ```
    stations = [
        %Pollutiondb.Station{name: "s1", lon: 1.1, lat: 1.1},
        %Pollutiondb.Station{name: "s2", lon: 2.1, lat: 2.1},
        %Pollutiondb.Station{name: "s3", lon: 3.1, lat: 3.1},
        %Pollutiondb.Station{name: "s4", lon: 4.1, lat: 4.1}
    ]
    ```
5. dodaj w module `Pollutiondb.Station` funkcje `getAll()`, `getById(id)` i `remove(station)` używając:
    ```
    Pollutiondb.Repo.get(Pollutiondb.Station, id)  
    Pollutiondb.Repo.all(Pollutiondb.Station)
    Pollutiondb.Repo.delete(station)
    ```
6. przetestuj wszystkie w iex; warto wykonać funkcję recompile by nie restartować iex

### Wyszukiwanie danych
1. w module Pollutiondb.Station dodaj funkcję `findByName(name)` używając:
    ```
    Pollutiondb.Repo.all( 
        Ecto.Query.where(Pollutiondb.Station, name: ^name) )
    ```
2. by makro Ecto.Query.where zadziałało poprawnie do nagłówków pliku dodaj:
    ```
    require Ecto.Query
    ```
3. w module Pollutiondb.Station funkcję `findByLocation(lon, lat)` używając:
    ```
    Ecto.Query.from(s in Pollutiondb.Station, 
        where: s.lon == ^lon,
        where: s.lat == ^lat)
        |> Pollutiondb.Repo.all
    ```
4. w module Pollutiondb.Station dodaj funkcję `findByLocationRange(lonMin, lonMax, latMin, latMax)`
5. przetestuj wszystkie funkcje
6. więcej szczegółów: https://hexdocs.pm/ecto/Ecto.Query.html

### Modyfikacja danych
1. modyfikacje wykonywane są przy pomocy tzw changesets; zdefiniuj w funkcji `updateName(station, newname)` w module `Pollutiondb.Station`:
    ```
    station
    |> Ecto.Changeset.cast(%{name: newname}, [:name])
    |> Ecto.Changeset.validate_required([:name])
    |> Pollutiondb.Repo.update
    ```
2. więcej opcji walidacji danych: https://hexdocs.pm/ecto/Ecto.Changeset.html
3. changeset może być też przekazany do `Pollutiondb.Repo.insert`; dodaj funkcję `add(name, lon, lat)`, która będzie sprawdzała czy wszystkie wartości są wypełnione i czy współrzędne mieszczą się w odpowiednich zakresach; potok zacznij od pustej struktury
4. walidację przenieś do osobnej funkcji `defp changeset(station, changesmap)`, użyj ją w add i w `updateName`

### Model dla relacji
1. dodaj plik `reading.ex` w `lib/pollutiondb`
2. zdefiniuj w nim moduł `Pollutiondb.Reading`, a w nim schma `readings` zawierające datę, czas, typ i wartość (https://hexdocs.pm/ecto/Ecto.Schema.html#module-types-and-casting)
3. dodaj do schematu również pole relacji:
    ```
    belongs_to :station, Pollutiondb.Station
    ```
4. w strukturze `Pollutiondb.Station` dodaj pole relacji:
    ```
    has_many :readings, Pollutiondb.Reading
    ```
5. utwórz nową migrację, która będzie definiowała nową tabelę `:readings` zawierającą 4 odpowiednie pola oraz identyfikator dla relacji:
    ```
    add :station_id, references(:stations, on_delete: :delete_all)
    ```
6. uruchom migrację, zbuduj i uruchom projekt
7. załaduj dane stacji pomiarowych - powinny zawierać informacje o pomiarach.

### Dane w relacji
1. w module `Pollutiondb.Reading` dodaj funkcję `addNow(station, type, value)` używając:
    ```
    date: Date.utc_today,
    time: Time.utc_now, 
    ```
2. dodaj kilka przykładowych wartości do kilku wybranych stacji
    * popraw błąd zgodnie z sugestią …
    * tworzone odczyty powinny zawierać relację do stacji
3. w module `Pollutiondb.Reading` dodaj funkcję `findByDate(date)`, przetestuj jej działanie
4. więcej szczegółów: https://hexdocs.pm/ecto/2.2.11/associations.html, https://hexdocs.pm/ecto/associations.html

### Ładowanie danych z pliku
1. wyczyść bazę, wywołując:
    ```
    mix ecto.drop
    mix ecto.create
    mix ecto.migrate
    ```
2. w module `Pollutiondb.Reading` dodaj funkcję `add(station, date, time, type, value)`
3. dodaj do projektu moduł ładujący dane z poprzednich zajęć oraz plik z danymi
4. zmodyfikuj moduł tak, by używał funkcji `Pollution.Station.add/3` oraz `Pollutiondb.Reading.add/5`
5. załaduj dane

## Zadanie domowe
1. dokończ zadania z zajęć
2. zabezpiecz zbudowaną aplikację, przygotuj ją na następne laboratorium
