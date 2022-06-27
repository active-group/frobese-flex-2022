-module(intro).
-export([welcome/0, sum/2]).

welcome() ->
    'welcome'.



sum(A,B) ->
   Z = A + B, 
   io:fwrite("~w~n",[Z]). 


