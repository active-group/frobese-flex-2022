-module(arr).
-export([rev/1,append/1,append/2,append_two/2,reverse/1,rev2/1,rev2/2]).

-spec rev(list(A)) -> list(A).
rev([]) -> [];
rev([F|R]) ->
    rev(R) ++ [F].



-spec append(list(A),list(A)) -> list(A).
append(L) -> append(L,[]).

append([],R) -> R;
append([H|T],R) -> append(T,[H|R]).

-spec append_two(list(A),list(A)) -> list(A).
append_two([],L2) -> L2;
append_two([FirstL1|RestL1], L2) -> [FirstL1 | append_two(RestL1, L2)].

reverse([]) ->[];
reverse([F|R]) -> append_two(reverse(R), [F]).


% Liste umdrehen
-spec rev2(list(A)) -> list(A).
% quadratisch
% rev([]) -> [];
% rev([First | Rest]) -> rev(Rest) ++ [First].
% Acc ist die Liste der bisher gesehenen Elemente, umgedreht.
rev2(List) -> rev2(List, []).
rev2([], Acc) -> Acc;
rev2([First|Rest], Acc) ->
  % tail call
  rev2(Rest, [First | Acc]).