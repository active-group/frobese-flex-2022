-module(intro).
-export([double/1]). 
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
double(X) -> X * 2.