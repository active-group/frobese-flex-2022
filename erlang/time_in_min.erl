-module(time_in_min).
-export([t1/0,time_in_min/1]).
-record(time, {hour :: 0..23, minute :: 0..59}).
t1() -> #time{ hour = 11, minute = 34}.
-spec time_in_min(#time{}) -> non_neg_integer().
time_in_min(#time{ hour = H, minute = M }) ->
    HM = H * 60,
    HM + M.
