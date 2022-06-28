-module(sum).
-export([sum/2]).

sum(A,B) ->
   Z = A + B, 
   io:fwrite("~w~n",[Z]). 
