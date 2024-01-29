# Laboratorium nr 5 - podstawy języka Elixir

## Przed zajęciami proszę o:
1. Uruchomienie interpretera `iex` lub `LiveBook`
2. Zapoznanie ze składnią Elixira:
    * Atomy, zmienne, `{kro,tki}`, `[lis,ty]`, `%{ma: :py}`, ...
    * Podstawowe operatory matematyczne, logiczne, ...
    * Moduły, funkcje, ...
3. Zaimplementowanie w pliku i przetestowanie w interpreterze modułu, który zawiera funkcję liczącą silnię.

## Cele ćwiczenia
1. Zapoznanie ze składnią języka Elixir,
2. Poznanie koncepcji potoków,
3. Integracja Elixira i Erlanga.


## Przebieg zajęć
W ramach zajęć zrealizujemy funkcjonalność parsowania danych o zanieczyszczeniu powietrza z pliku i ich ładowania do modułu `pollution_server`. Plik zawiera w kolejnych wierszach następujące dane:
```
03-05-2017,00:02,19.992,50.081,96
03-05-2017,00:58,19.727,49.989,94
03-05-2017,00:15,19.678,49.989,76
```

czyli:

```
DATA,GODZINA,DLUGOSC,SZEROKOSC,WARTOSC
```

Przy implementacji korzystaj z operatora strumieni `|>` kod będzie znacznie łatwiejszy do zrozumienia.

Przed przystąpieniem do implementacji przeczytaj cały punkt, w wielu miejscach sugerowane są ułatwienia.

Kolejne punkty realizuj w osobnych blokach kodu.
1. Import z pliku. Załaduj dane do listy, przyda się:
    * `File.read!(„file.txt”)`
    * `String.split(„string”, „delimiter”)`
    * … warto użyć: `File.read!(…) |> String.split(…)`
    * By sprawdzić co się załadowało użyj `length(list)` - w pliku jest nieco ponad 5900 wpisów
2. Parsowanie danych. Zdefiniuj moduł i napisz funkcję konwertującą jedną linijkę danych do postaci: `%{:datetime ⇒ { {2017, 5, 22}, {13, 23, 30} }, :location ⇒ {19.992,50.081}, :pollutionLevel ⇒ 96}`. Przydadzą się:
    * `[pattern, matching, on, list] = String.split(...`
    * `Enum.reverse`
    * `Enum.map(list, fn)`
    * `Integer.parse(„1234”)`, `Float.parse(„1234.3”)`
    * `elem(tuple, 0)`
    * `:erlang.list_to_tuple`
    * ...tu również warto użyć: `date |> …split… |> …reverse… |> …map(elem(parse))…`
    * ...można zdefiniować funkcje pomocnicze
3. Identyfikacja stacji pomiarowych. Dane załadowane do struktur trzeba przeanalizować w celu wykrycia stacji pomiarowych. Utwórz funkcję `identifyStations`, która zwróci dane potrzebne do utworzenia stacji.
    * można użyć: `Enum.uniq_by(enumerable, fun)`
    * ile jest unikatowych stacji w danych? Powiedz na głos, jeśli obliczysz tą wartość.
4. Startowanie aplikacji `pollution_app`
    * dodaj ścieżkę do zbudowanej aplikacji do interpretera, używając `Code.append_path(„Path/To/app/ebin”)`
    * wystartuj aplikację używając `Application.start(:pollution_app)`
5. Ładowanie danych stacji. Utwórz stacje, generując ich nazwy w postaci `„station_longitude_latitude”`. Przyda się:
    * `string = „ala ma #{catsCount} kotow”`
6. Ładowanie danych pomiarów. Załaduj dane wszystkich pomiarów, przyjmując rodzaj zanieczyszczeń jako `„PM10”`. Zmierz czas ładowania, wyniki wpisz do arkusza. Można użyć:
    * `fn |> :timer.tc |> elem(0)`
7. Analiza danych. Zmierz wartości wybranych analiz parametrów zanieczyszczeń i wpisz je do arkusza. Przetestuj funkcje analizujące w swojej implementacji korzystając z przykładowych wywołań w arkuszu.
8. Modyfikacja ładowania [zadanie dodatkowe]. Przeprowadź refaktoring, który wprowadzi wykorzystanie strumieni w ładowaniu danych. Porównaj czas ładowania. Wykorzystaj:
    * `File.stream!(filename)`
    * `Stream.map`, `Stream.flat_map`, `Stream.filter`
    * `stream |> Enum.reduce`

## Zadanie domowe
* Dokończ zadania z zajęć.
* Zbuduj moduł Elixirowy, który będzie zawierał wszystkie funkcje pozwalające na ładowanie danych z pliku.
