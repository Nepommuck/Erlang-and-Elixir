# Laboratorium 1

W laboratorium dostępne będą komputery z potrzebnym oprogramowaniem. Chcąc pracować na własnym komputerze, proszę zainstalować:

1. Erlang OTP 25.x
2. IntelliJ z wtyczką Erlang

## Cele zajęć
1. Zapoznanie z powłoką Erlanga
2. Poznanie podstaw składni języka
3. Programowanie modułów i funkcji

## Przebieg zajęć

### Eshell
1. Uruchom erl w konsoli
2. Przetestuj działanie podstawowych elementów składni:
    * Wyrażenia arytmetyczne na liczbach całkowitych i zmiennoprzecinkowych
        * 2+4.
        * 2.0+4.
        * 4 / 2.
        * 2 / 0.
        * 4 div 3.
        * 4.0 rem 1.1.
    * Zmienne
        * A = 2 + 2. B = A + 2.
        * C = ala. D = makota.
        * 6 = B.
        * B = 12.
    * Operatory logiczne
        * B == 6. A =:= B.
3. Krotki i listy
    1. Zaproponuj strukturę, w której będzie się dało przechowywać odczyt ze stacji pomiaru jakości powietrza. Każdy pomiar ma zawierać nazwę stacji, czas wykonania i wartości pomiarów. Różne stacje są wyposażone w różną liczbę czujników równych rodzajów, np PM10, PM2.5, PM1, temperatura, ciśnienie, wilgotność, …
    2. Utwórz przykładowe zmienne `P1`, `P2`, `P3` związane z trzema przykładowymi pomiarami.
    3. Utwórz zmienną `ListaPomiarow` związaną z listą pomiarów.
    4. Utwórz jeszcze jedną zmienną `P4` związaną z innym pomiarem
    5. Wykorzystując tylko zmienne `ListaPomiarow` oraz `P4` utwórz listę `NowaListaPomiarow` zawierającą wszystkie produkty. Użyj definicji rekurencyjnej listy.
    6. Napisz wyrażenie, które ze zmienną `NazwaP1` zwiąże nazwę stacji w zmiennej `P1`.
4. Makra powłoki, funkcje wbudowane i moduły standardowe
    * `f().` - czyści wszystkie wiązania zmiennych.
    * `time().` Zwiąż zmienną o nazwie `Minutki` z aktualną minutą.
    * `list_to_tuple(ListaPomiarow).`
    * `io:format(„Stacja nazywa się ~p.~nAktualna minutka to ~B.~n”, [NazwaP1, Minutki] ).`

### Moduły i funkcje
1. Uruchom `IntelliJ`, załóż nowy projekt Erlang,
2. Utwórz nowy moduł, czyli nowy plik z rozszerzeniem `.erl` w katalogu `src`
3. W module zaimplementuj funkcję `power/2`, która podniesie pierwszy argument do potęgi podanej w drugim parametrze.
4. Przetestuj jej działanie w konsoli; w tym celu utwórz nową konfiguracje uruchomieniową typu `Erlang Console`.
5. Utwórz nowy moduł o nazwie `myLists`. Zaimplementuj i przetestuj funkcje:
    * `contains/2`, która jako parametry weźmie listę i wartość, i zwróci true jeśli lista zawiera wartość.
    * `duplicateElements/1`, która zwróci listę zawierającą każdy z elementów dwukrotnie - `[A, B, …]` zmienia w `[A, A, B, B, …]`.
    * `sumFloats/1`, która zsumuje elementy będące liczbami zmiennoprzecinkowymi.
6. Zmodyfikuj funkcję `sumFloats/1` by korzystała z rekurencji ogonowej.

### Kalkulator ONP
* http://pl.wikipedia.org/wiki/Odwrotna_notacja_polska
* Zapisz w onp następujące wyrażenia :
    * `1 + 2 * 3 - 4 / 5 + 6`
    * `1 + 2 + 3 + 4 + 5 + 6 * 7`
    * `( (4 + 7) / 3 ) * (2 - 19)`
    * `17 * (31 + 4) / ( (26 - 15) * 2 - 22 ) - 1`
* Utwórz moduł onp z funkcją `onp/1`, która dla poprawnego wyrażenia ONP zwróci wynik
    * Funkcja powinna obsługiwać operacje `+`, `-`, `*`, `/` oraz liczby całkowite
    * do parsowania wyrażenia wykorzystaj funkcję `string:tokens/2` lub `string:lexemes/2`
    * do konwertowania ciągów znaków na liczby wykorzystaj funkcję `string:to_integer/1`
    * sprawdź działanie dla zapisanych wyrażeń (https://k144.github.io/narz%C4%99dzia/onp/)

### Zadanie domowe
1. Dokończ zadania z zajęć.
2. Dodaj do kalkulatora operacje `sqrt`, `pow` i funkcje trygonometryczne
3. Dodaj do kalkulatora obsługę liczb zmiennoprzecinkowych w wyrażeniach
