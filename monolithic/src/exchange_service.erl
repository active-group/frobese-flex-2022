-module(exchange_service).
-include("data.hrl").

-export([exchange/2, knows_currency/1]).

-spec string_to_num(string()) -> number().
string_to_num(N) ->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

extract_currencies([]) -> [];
extract_currencies([X | XS]) ->
    case X of
        " " -> extract_currencies(XS);
        {'Cube', CR, _} -> [CR | extract_currencies(XS)]
    end.

parse_currencies(Raw) ->
    lists:map(fun([{_, Currency}, {_, Rate}]) ->
                      {Currency, string_to_num(Rate)}
              end, Raw).

parse_xml_content({_, _, Content}) ->
    %% dont do this at home :)
    [_,_,_,_,_,{_, _, [_ , {_, _, R} | _]} | _] = Content,
    Raw = extract_currencies(R),
    Parsed = parse_currencies(Raw),
    maps:from_list(Parsed).


fetch() ->
    R = httpc:request("https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml"),
    case R of
        {ok, {{_, 200, _}, _, Body}} ->
            {XML, _} = xmerl_scan:string(Body, [{space, normalize}]),
            XMLSimplified = xmerl_lib:simplify_element(XML),
            lager:info("Fetched exchange rates from ecb", []),
            parse_xml_content(XMLSimplified);
        E ->
            lager:error("Error in request to ecb: ~p~n", [E]),
            {}
    end.


exchange_helper(Symbol, Amount) ->
    case maps:get(Symbol, fetch(), undefined) of
        undefined -> {error, currency_not_found};
        Rate -> {ok, Rate * Amount}
    end.


-spec exchange(string(), money()) -> {ok, money()} | {error, currency_not_found}.
exchange(Symbol, Amount) ->
    case Symbol of
        "EUR" -> {ok, Amount};
        _ -> exchange_helper(Symbol, Amount)
    end.

-spec knows_currency(string()) -> boolean().
knows_currency(Symbol) ->
    case Symbol of
        "EUR" -> true;
        _ ->
            case exchange_helper(Symbol, 0) of
                {error, currency_not_found} -> false;
                _ -> true
            end
    end.
