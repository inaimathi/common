-module(common).
-export([now_to_seconds/1, binary_to_hex/1, make_tempname/0, make_tempname/1, rec_to_proplist/2, get_values/2]).

now_to_seconds(Now) ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(Now)).

binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

make_tempname() ->
    make_tempname(filename:nativename("/tmp")).
make_tempname(TargetDir) ->
    {A, B, C} = now(),
    [D, E, F] = lists:map(fun integer_to_list/1, [A, B, C]),
    Tempname = lists:append(["tmp.", D, ".", E, ".", F]),
    filename:absname_join(TargetDir, Tempname).

rec_to_proplist(Rec, Fields) ->
    [_RecName | Values] = tuple_to_list(Rec),
    lists:zip(Fields, Values).

get_values(Keys, Proplist) -> 
    lists:map(fun (K) -> proplists:get_value(K, Proplist) end, Keys).
