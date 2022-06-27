-module(intro).
-export([double/1, double/2, state/1, typical/1, dogs_per_leg/1,
         t1/0, t2/0, minutes_since_midnight/1]). 
% double/1 die einstellige Funktion double

% ^^^  sollte dem Dateinamen entsprechen
% 
% Alles mit Punkt abschließen
% 42 Zahl
% mike Atom (Kleinbuchstabe)
% "mike" (String)
% {1, "mike", sperber} Tupel
% <<1,2,3>> Binary: Bytes 1 2 3
% <<"mike">> Binary, mit ASCII-Codierung
% [1,2,3] Liste
% Anmerkung: Strings sind Listen
% #{a => "hello"}. Map
% #{a => "hello", b => "goodbye"}.
% Variablen fangen mit Großbuchstaben an
% 
% Liste ist eins der folgenden:
% - die leere Liste
% - eine Cons-Liste aus erstem Element und Rest
-spec double(number()) -> number().
double(X) -> X * 2.

-spec double(number(), number()) -> number(). 
double(X, Y) -> double(X * Y).

-type st() :: fest | fluessig | gas.

% Aggregatzustand von Wasser
-spec state(number()) -> st().
state(Temp) ->
    if 
        Temp < 0 -> fest;
        Temp > 100 -> gas; % ; dazwischen
        true -> fluessig
    end.

% fest | fluessig | gas: Fallunterscheidung
% Eine Funktionsklausel pro Fall. 

% Typische Temperatur für Aggregatzustand
-spec typical(st()) -> number() | error.
% Pattern Matching, Reihenfolge wichtig

% typical(fest) -> -20;
% typical(fluessig) -> 20;
% typical(gas) -> 100.
% 
typical(State) ->
    case State of
        fest -> -20;
        fluessig -> 20;
        gas -> 100;
        _ -> error
    end.

safe_divide(X, Y) ->
    if
        Y == 0 -> {error, divide_by_zero};
        true -> {ok, X / Y}
    end.

dogs_per_leg(Legs) ->
    case safe_divide(Legs, 0) of
        {error, Message} -> 
            io:format("problem: ~w~n", [Message]), % Komma!
            {error, bug_in_code};
        {ok, Dogs} -> Dogs
    end.

% 3 Sorten Haustiere:
% Hund, Katze, Schlange
% Schreibe eine Funktion, die liefert, ob ein Haustier niedlich ist.

% Eine Uhrzeit besteht aus:
% - Stunde
% - Minute
% zusammengesetzte Daten
-record(time, {hour :: 0..23, minute :: 0..59}).

t1() -> #time{ hour = 11, minute = 34}.
t2() -> #time{ hour = 12, minute = 12}.

% Minuten seit Mitternacht
-spec minutes_since_midnight(#time{}) -> non_neg_integer().
minutes_since_midnight(#time{ hour = H, minute = M }) ->
    HM = H * 60,
    HM + M.

% Ein Gürteltier hat folgende Eigenschaften:
% - tot oder lebendig
% - Gewicht
% zusammengesetzte Daten
-record(dillo, { liveness :: dead | alive, weight :: number() }).

% Gürteltier überfahren
-spec run_over_dillo(#dillo{}) -> #dillo{}.
run_over_dillo(#dillo{ liveness = Liveness, weight = Weight}) ->
    #dillo{ liveness = dead, weight = Weight}
