-module(number_formatter).

-export([format/2]).
-export_type([locale/0]).

-type locale() :: en | de.

part(List) ->
    part(List, []).
part([], Acc) ->
    lists:reverse(Acc);
part([H], Acc) ->
    lists:reverse([[H]|Acc]);
part([H1,H2,H3|T], Acc) ->
    part(T, [[H1,H2, H3]|Acc]);
part([H1,H2|T], Acc) ->
    part(T, [[H1,H2]|Acc]).

r(Number) ->
    round(Number * 100) / 100.

sep(Str, ThousandSep, DecimalSep) ->
    [B, F] = string:split(Str, "."),
    Formatted = B ++ DecimalSep ++ string:join(part(F), ThousandSep),
    lists:reverse(Formatted).

format_separated(N, ThousandSep, DecimalSep) ->
    Rounded = r(N),
    RoundedStr = io_lib:format("~.2f", [Rounded]),
    Rev = lists:reverse(RoundedStr),
    sep(Rev, ThousandSep, DecimalSep).

%% @doc Prints a number, localized, rounded to 2 decimal digits, with 1000s separators.
-spec format(locale(), number()) -> string().
format(en, Number) -> format_separated(Number, ",", ".");
format(de, Number) -> format_separated(Number, ".", ",").
