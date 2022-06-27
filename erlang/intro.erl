-module(intro).
-export([welcome/0, sum/2,pets/1]).

welcome() ->
    'welcome'.



sum(A,B) ->
   Z = A + B, 
   io:fwrite("~w~n",[Z]). 



-type pets_type() :: cat | snake | dog.

-spec pets(pets_type()) -> neidig | lieb.
pets(Pets_type) ->
    case Pets_type of
        cat -> 'neidig';
        snake -> 'neidig';
        dog -> 'lieb'
    end.