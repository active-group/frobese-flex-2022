-module(counter).
-export([init/1, handle_cast/2, handle_call/3,
         start/1, inc/2, get/1]).

-behavior(gen_server).

start(Start) ->
    gen_server:start(?MODULE, Start, [{debug, [trace]}]).



-record(inc, { bump :: number() }).

inc(Pid, Bump) ->
    gen_server:cast(Pid, #inc { bump = Bump}).

init(Start) ->
    {ok, Start}. % Start ist der initiale Wert für N


handle_cast(#inc{ bump = Bump}, N) ->
    {noreply, N + Bump}.

-record(get, {}).
get(Pid) ->
    gen_server:call(Pid, #get{}).

handle_call(#get{}, _From, N) ->
    {reply, N, N}.