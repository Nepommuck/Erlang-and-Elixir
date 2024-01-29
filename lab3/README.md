# Laboratorium 3 - programowanie procesów w Erlangu

## Przed zajęciami

Zapoznaj się z

1. Tworzeniem procesów.
2. Rejestracja procesów.
3. Komunikacja między procesami.

## Rozgrzewka
W Eshellu
```
%%Tworzenie procesów.
>
> spawn(fun() -> io:format("To jest tekst~n") end).
>
> F = ... %definicja Twojego funa!
> spawn(F).
%% Przesyłanie wiadomości
> Proces1 = spawn(fun() -> 
>   receive 
>     _ -> io:format("Otrzymalem wiadomosc!~n") 
>   end 
> end).
> Proces1 ! ... % wyślij jakąś wiadomość
> Proces1 ! ... % jeszcze raz
% Co się stało ? I dlaczego ?
...
%% Teraz stworzymy proces który przetrzymuje jakiś stan.
%% By to zrobić, wpierw musimy zdefinować funkcjonalność procesu.
% Funkcjonalność procesu
> IncLoop = fun Loop(N) ->
>   receive
>     inc   -> Loop(N+1);
>     print -> io:format("~B~n",[N]), Loop(N);
>     stop  -> ok
>   end
> end.
>
%% Tworzenie procesu
%% Zwróć uwagę na funa... 
> Proces2 = spawn(fun() -> IncLoop(0) end).
%% Czy poniższa linijka zadziała ?
> Proces3 = spawn(IncLoop).
% Dlaczego ?
...
%% Co robi ta linijka ?
> register(proces_stanu, Proces2).
%% Teraz mamy bardziej skomplikowany proces, który zarządza jakimś stanem
%% Co jest tym stanem ?
...
%% Co możemy robić z tym stanem ?
...
%% Komunikacja z procesem stanu.
> Proces2 ! inc.
> proces_stanu ! print.
> proces_stanu ! inc.
> Proces2 ! print.
```

## Cele zajęć
1. Poszerzenie wiedzy dotyczącej Erlangowych procesów.
2. Poznanie sposobów łączenia węzłów zdalnych

Celem zajęć jest zapoznanie się z procesami w Erlangu, poznanie mechanizmów ich tworzenia, rejestracji, i komunikacji.

## Przebieg zajęć

### Tests
1. Pobierz plik z testami modułu pollution
2. wypakuj w miejsce, w którym znajduje się Twoja implementacja modułu pollution
3. oba modułu skompiluj używając: `c(pollution).`, `c(pollution_test).`
4. uruchom `eunit:test([pollution_test]).`

### Ping - Pong
1. Napisz moduł pingpong, który będzie eksportował funkcje:
    * `start/0`, która utworzy 2 procesy i zarejestruje je pod nazwami `ping` i `pong`,
    * `stop/0`, która zakończy oba procesy,
    * `play/1`, która wyśle wiadomość z liczbą całkowitą N do procesu `ping`.
2. Po otrzymaniu wiadomości, proces `ping` ma rozpocząć wymianę N wiadomości z procesem `pong`. Przy odebraniu każdej wiadomości procesy mają wypisać na standardowe wyjście informację o przebiegu odbijania.
3. Dla zwiększenia czytelności działania warto użyć funkcji `timer:sleep(Milisec)`.
4. Procesy `ping` i `pong` powinny samoczynnie kończyć działanie po 20 sekundach bezczynności.
5. Zmodyfikuj proces `ping` by przechowywał stan - sumę wszystkich liczb z komunikatów, które otrzymał. Dodaj tę sumę do informacji wypisywanej po odebraniu komunikatu.

### Obliczenia równoległe
Dane są 2 listy punktów na płaszczyźnie 2D:

1. lista lokalizacji paczkomatów, których jest np. `1000`
2. lista rozmieszczenia osób, które chcą odwiedzić paczkomat - jest ich np. `20000`

Szukamy osoby, która jest najbliżej dowolnego paczkomatu.

1. Przygotuj dane - 2 listy par liczb całkowitych z zakresu `0` - `10000`, wygenerowane losowo. Użyj list comprehensions.
2. Napisz pomocniczą funkcję `dist({X1, Y1}, {X2, Y2})`, która zwróci dystans między punktami.

Wersja sekwencyjna:

1. Napisz funkcję `findMinDistance(PeopleLocations, LockerLocations)`, która wyszuka osobę i zwróci `{dystans, {pozycjaOsoby, pozycjaPaczkomatu}}`. Do wyznaczenie wszystkich dystansów można użyć list comprehension. Wyszukanie najbliższej może zrealizować funkcja `lists:min`.
2. Zmierz czas wykonania funkcji

Wersja bardzo równoległa:

1. Napisz wersję funkcji `findMinDistance(PeopleLocations, LockerLocations, ParentPID)`, która po obliczeniu wyniku odsyła go do podajego Pid.
2. Napisz funkcję, która uruchomi wyszukiwanie w osobnym procesie dla każdej osoby oddzielnie.
3. Odbierz komunikaty, zbierz wszystkie wyniki od procesów w liście.
4. Zarówno tworzenie procesów jak i odbieranie wyników można zrealizować używając list comprehension.
5. Wyszukanie wyniku może zrealizować funkcja `lists:min`.
6. Porównaj czas obliczeń.

Wersja mniej równoległa (zadanie dodatkowe):
1. Uruchom tyle procesów wywołujących funkcję findMinDistance, ile rdzeni ma twój procesor. Każdy powinien dostać odpowiednią część zadania - podzbiór osób.
2. Porównaj czas obliczeń.

### Distributed Erlang
W tej części trzeba będzie skorzystać z maszyn w laboratorium.

1. uruchom węzeł erlanga, spróbuj połączyć się z węzłem uruchomionym przez prowadzącego
    * przyda się funkcja `net_adm:ping(NodeName)` oraz `BIF nodes()`.
2. napisz… szczegóły na laboratorium…

### Serwer zanieczyszczeń
1. Zaimplementuj moduł `pollution_server`, który będzie startował proces obsługujący funkcjonalność modułu `pollution`. Powinien działać analogicznie do serwera zmiennej globalnej - o bogatszej funkcjonalności.
2. Dodatkowe funkcje eksportowane: `start/0` i `stop/0`.
3. Dodatkowa funkcja: `init/0`, która będzie wykonywana już w kontekście nowego procesu.
4. Serwer powinien dostarczyć funkcje analogiczne do modułu `pollution` - ale każda z nich będzie miała o jeden argument mniej. Serwer ma wołać funkcje z modułu `pollution`.

## Zadanie domowe
1. Dokończ moduł `pollution_server`
2. Przetestuj moduły `pollution` i `pollution_server`.
