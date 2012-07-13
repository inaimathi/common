-module(db).
-include_lib("stdlib/include/qlc.hrl").

-export([do/1, atomic_insert/1, transaction/1]).
-export([relate/6, decouple/6, push_to/3, remove_from/3]).

atomic_insert(Records) when is_list(Records) ->
    transaction(fun() -> lists:map(fun mnesia:write/1, Records) end);
atomic_insert(Rec) -> 
    transaction(fun() -> mnesia:write(Rec) end).

do(Q) -> transaction(fun() -> qlc:e(Q) end).

transaction(F) ->
    {atomic, Val} = mnesia:transaction(F),
    Val.

relate(Rec1, Id1, Index1, Rec2, Id2, Index2) ->
    F = fun() -> mnesia:write(push_to(Index1, Rec1, element(Id2, Rec2))),
		 mnesia:write(push_to(Index2, Rec2, element(Id1, Rec1)))
	end,
    transaction(F).

decouple(Rec1, Id1, Index1, Rec2, Id2, Index2) ->
    F = fun() -> mnesia:write(remove_from(Index1, Rec1, element(Id2, Rec2))),
		 mnesia:write(remove_from(Index2, Rec2, element(Id1, Rec1)))
	end,
    transaction(F).

push_to(Index, Rec, NewVal) -> 
    Val = element(Index, Rec),
    setelement(Index, Rec, [NewVal | Val]).

remove_from(Index, Rec, Prey) ->
    Val = element(Index, Rec),
    setelement(Index, Rec, lists:delete(Prey, Val)).