# Laboratorium nr 4 - wzorce projektowe i OTP


## Przed zajęciami
1. Wykonaj wprowadzenie do OTP.
2. Przeczytaj wprowadzenie do Rebara.

## Cele zajęć
1. Poznanie narzędzia `rebar3`.
2. Zapoznanie z wzorcami projektowymi OTP: `gen_server`, `supervisor`, `gen_statem`, `application`

## Przebieg zajęć

### rebar
1. Przy pomocy `rebar3` utwórz nowy projekt typu `app` w wybranym folderze.
2. Przeanalizuj strukturę generowane przez rebar.
3. Otwórz projekt w `IntelliJ` metodą `New → Project From Existing Sources`...
    * wskaż folder z wygenerowanymi źródłami,
    * wybierz opcję `Create project from existring sources`,
    * nie zmieniaj jego lokalizacji.
4. Skonfiguruj środowisko do współpracy z rebarem.
    * w `Settings → Other Settings → Erlang External Tools` skonfiguruj `rebar3`
    * w `Settings → Build… → Compiler → Erlang Compiler` włącz budowanie rebarem
5. Dodaj i uruchom konfigurację uruchomieniową typu Rebar startującą polecenie shell.
    * inne polecenia rebara: https://www.rebar3.org/docs/commands
6. Uruchom `observer:start().` i sprawdź czy Twoja aplikacja jest uruchomiona.
7. W konsoli możesz wywołać `application:stop(nazwaAplikacji).`

### gen_server
1. Dodaj do źródeł projektu moduł `pollution_gen_server`. Wykorzystaj szkielet OTP `gen_server minimal`.
2. Zmodyfikuj `pollution_gen_server` by udostępniał API zgodne z modułem pollution z poprzednich zajęć:
    * `addStation/2`
    * `addValue/4`
    * `getOneValue/3`
    * ...pozostałe funkcje modułu pollution, ale wcześniej:
3. Dodaj do interfejsu serwera funkcję `crash/0`, która spowoduje wykonanie niepoprawnej operacji w procesie serwera.

### supervisor
1. Dodaj `pollution_gen_server` do listy serwerów startowanych i nadzorowanych przez domyślnego supervisora.
2. Sprawdź teraz działanie funkcji `crash/0`.
3. Co dzieje się z danymi w przypadku awarii serwera? Jak można naprawić ten problem?

### gen_statem
1. Dodaj moduł `pollution_value_collector_gen_statem`, który będzie pozwalał na dodawanie wielu danych do ustalonej stacji poprzez kolejne wywołania funkcji.
2. Moduł realizuje wzorzec OTP `gen_statem`; API modułu:
    * `start_link()`, `stop()`
    * `set_station(…)`
    * `add_value(…)`
    * `store_data()`
3. Trzy powyższe funkcje muszą być wykonane w kolejności, przy czym `add_value` może być wykonana wiele razy.
4. Dopiero po wykonaniu `store_data` wszystkie pomiary są dodawane do serwera zanieczyszczeń, a `pollution_value_collector` wraca do stanu oczekiwania na wybór stacji.

## Zadanie domowe
1. Dokończ budowanie kompletnej aplikacji.
2. Sprawdź czy poprawnie działa i poprawnie obsługuje błędy.
