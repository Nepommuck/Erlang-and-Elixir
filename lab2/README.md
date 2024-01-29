# Laboratorium 2

## Przed zajęciami

Zapoznaj się z:

1. Funkcjami wyższego rzędu.
    1. składnia `fun … end`
    2. Funkcja jako argument innej funkcji.
    3. `lists:map/2`, `lists:filter/2`, `lists:foldl/3`
2. List comprehensions.
3. Rekordami i mapami.

## Cele zajęć

Na dzisiejszych zajęciach poznamy funkcyjne sposoby rozwiązywania problemów w Erlangu. Wykorzystamy do tego list comprehensions jak i funkcje wyższego rzędu.

Część problemów do których rozwiązania używamy pętli `for` czy `while` można również wyrazić za pomocą powyższych narzędzi.

Zapoznamy się również z rekordami i zbudujemy moduł o zadanej funkcjonalności.

## Rozgrzewka

Tą część wykonujemy w shellu Erlnga
```
%%Proste list comprehensions.
>[X*2 ||  X<- lists:seq(1,10), X rem 2 == 0].
>[rand:uniform(25)+97 || _<-lists:seq(1,30)].
>[ [rand:uniform(25)+97 || _<-lists:seq(1,rand:uniform(10))] || _<-lists:seq(1,20)].

%%Definicja funa.
> Hello = fun(X) -> io:format("Hello ~s~n", [X]) end.
%%Wywołanie funa.
> Hello("World").

%%Fun jako argument funkcji wyższego rzędu
> Add1 = fun(X) -> X + 1 end.
%Co zwrócą poniższe wyrażenia ?
> lists:map(Add1, [1,2,3]).
> lists:map(fun(X) -> X+2 end, [1,2,3]).
> lists:map(fun math:sqrt/1, [1,2,3]).

%%Zdefiniuj własnego funa, który mnoży dany argument razy 2.
> Multi2 = ...
% I przetestuj tak jak powyżej
> lists:map(?, ?).
```

## QuickSort
Tą część wykonujemy w IntelliJ
Celem tego ćwiczenia będzie zaimplementowanie algorytmu quicksort. W module `qsort` utwórz następujące funkcje:

1. Funkcja 1, która dla listy i zadanego argumentu wybierze te elementy które są mniejsze od argumentu. Wykorzystaj list comprehensions.
    ```
    less_than(List, Arg) -> ... 
    ```
2. Funkcja `grt_eq_than/2`, która dla listy i zadanego argumentu wybierze te elementy które są większe bądź równe od argumentu. Tutaj też wykorzystaj list comprehensions.
    ```
    grt_eq_than(List, Arg) -> ... 
    ```
3. Funkcja qs/1 implementująca algorytm quicksort:
    ```
    qs([Pivot|Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ) 
    ```
Mając zaimplementowanego quicksorta, dobrze byłoby sprawdzić jego działanie. W tym celu zaimplementuj funkcje pomocnicze ułatwiające testowanie.

1. Funkcja `random_elems/3`, która zwróci listę losowych elementów z zakresu [Min,Max] o rozmiarze N. Wykorzystaj list comprehensions oraz `rand:uniform/1` i `lists:seq/2`.
    ```
    random_elems(N,Min,Max)-> ... 
    ```
2. Funkcja `compare_speeds/3` która porówna prędkości działania podanych algorytmów sortujących dla zadanej listy. Dwa ostatnie parametry to funkcje. Wykorzystaj do tego funkcję `timer:tc`
    ```
    compare_speeds(List, Fun1, Fun2) -> ... 
    ```

Interesujące nas dane wypisz na standardowe wyjście, formatując je funkcją `io:format/2`.

Następnie w Eshellu przetestuj funkcję `compare_speeds/3`, używając `qsort:qs/1` jak i `lists:sort/1`.

## Fun
Wszystkie zadania z tego punktu można wykonać w Eshellu.

1. Zdefiniuj funkcję anonimową, która w ciągu znaków podmieni wszystkie `a` na `e`, `e` na `o`, pozostałe litery pozostawi bez zmian
2. Zdefiniuj funkcję anonimową, która policzy ile liczb w zadanej liście jest podzielnych przez 3.
3. Stwórz funkcję anonimową, która policzy sumę cyfr w liczbie. Użyj do tego `lists:foldl/3`.

## Pollution
Utwórz nowy moduł o nazwie pollution, który będzie zbierał i przetwarzał dane ze stacji mierzących jakość powietrza. Moduł powinien przechowywać:

* informacje o stacjach pomiarowych,
    * współrzędne geograficzne,
    * nazwy stacji pomiarowych,
* zmierzone wartości pomiarów, np stężenia pyłów PM10, PM2.5 czy wartości temperatury (wraz z datą i godziną pomiaru).

Nie powinno być możliwe:
* dodanie dwóch stacji pomiarowych o tej samej nazwie lub tych samych współrzędnych
* dodanie dwóch pomiarów o tych samych:
    * współrzędnych,
    * dacie i godzinie,
    * typie (PM10, PM2.5, temperatura, …);
* dodanie pomiaru do nieistniejącej stacji.

Zaprojektuj strukturę danych dla przechowywania takich informacji (jest przynajmniej kilka dobrych rozwiązań tego problemu).

Zaimplementuj funkcje w module pollution:
* `create_monitor/0` - tworzy i zwraca nowy monitor zanieczyszczeń;
* `add_station/3` - dodaje do monitora wpis o nowej stacji pomiarowej (nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor;
* `add_value/5` - dodaje odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość), zwraca zaktualizowany monitor;
* `remove_value/4` - usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
* `get_one_value/4` - zwraca wartość pomiaru z zadanej stacji o zadanym typie i z zadanej daty;
* `get_station_mean/3` - zwraca średnią wartość parametru z zadanej stacji i danego typu;
* `get_daily_mean/3` - zwraca średnią wartość parametru danego typu, danego dnia na wszystkich stacjach;

W funkcjach używaj następujących typów i formatów danych:

* do przechowywania dat użyj struktur z modułu calendar (zob. `calendar:local_time()`),
* współrzędne geograficzne to para (krotka) liczb,
* nazwy, typy to ciągi znaków.

Przetestuj działanie modułu.

* `P = pollution:create_monitor().`
* `P1 = pollution:add_station(„Aleja Słowackiego”, {50.2345, 18.3445}, P).`
* `P2 = pollution:add_value({50.2345, 18.3445}, calendar:local_time(), „PM10”, 59, P1).`
* `P3 = pollution:add_value(„Aleja Słowackiego”, calendar:local_time(), „PM2,5”, 113, P2).`
* …

Zabezpiecz moduł pollution - będzie on potrzebny na następnych zajęciach.

## Zadanie domowe
1. Dokończ moduł pollution.
2. Napisz testy do modułu pollution z wykorzystaniem `EUnit` - nie jest to wymagane, ale może pomóc.
3. Niespodzianka!
