-module(intro).
-export([double/1, double/2, state/1, typical/1]). 
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

% Aggregatzustand von Wasser
-spec state(number()) -> fest | fluessig | gas.
state(Temp) ->
    if 
        Temp < 0 -> fest;
        Temp > 100 -> gas; % ; dazwischen
        true -> fluessig
    end.

% fest | fluessig | gas: Fallunterscheidung
% Eine Funktionsklausel pro Fall. 

% Typische Temperatur für Aggregatzustand
-spec typical(fest | fluessig | gas) -> number().
typical(fest) -> -20;
typical(fluessig) -> 20;
typical(gas) -> 100.
