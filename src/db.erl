-module(db).
-include_lib("stdlib/include/qlc.hrl").

-export([do/1, atomic_insert/1, transaction/1]).
-export([find/3, list/1, list/2, relate/6, decouple/6, push_to/3, remove_from/3]).

find(Table, Id, Value) ->
    try
	[Rec] = do(qlc:q([X || X <- mnesia:table(Table), element(Id, X) =:= Value])),
	Rec
    catch
	error:_ -> false
    end.

list(Table) ->
    db:do(qlc:q([X || X <- mnesia:table(Table)])).
list(Table, Fields) ->
    db:do(qlc:q([extract_fields(X, Fields) || X <- mnesia:table(Table)])).

do(Q) -> transaction(fun() -> qlc:e(Q) end).

transaction(F) ->
    {atomic, Val} = mnesia:transaction(F),
    Val.

atomic_insert(Records) when is_list(Records) ->
    transaction(fun() -> lists:map(fun mnesia:write/1, Records) end);
atomic_insert(Rec) -> 
    transaction(fun() -> mnesia:write(Rec) end).

push_to(Index, Rec, NewVal) -> 
    Val = element(Index, Rec),
    setelement(Index, Rec, [NewVal | Val]).

remove_from(Index, Rec, Prey) ->
    Val = element(Index, Rec),
    setelement(Index, Rec, lists:delete(Prey, Val)).

relate(Rec1, Id1, Index1, Rec2, Id2, Index2) ->
    case {lists:member(element(Id2, Rec2), element(Index1, Rec1)),
	  lists:member(element(Id1, Rec1), element(Index2, Rec2))}  of
	{false, false} -> F = fun() -> mnesia:write(push_to(Index1, Rec1, element(Id2, Rec2))),
				       mnesia:write(push_to(Index2, Rec2, element(Id1, Rec1)))
			      end,    
			  transaction(F);
	_ -> false
    end.
				  

decouple(Rec1, Id1, Index1, Rec2, Id2, Index2) ->
    case {lists:member(element(Id2, Rec2), element(Index1, Rec1)),
	  lists:member(element(Id1, Rec1), element(Index2, Rec2))}  of
	{true, true} -> F = fun() -> mnesia:write(remove_from(Index1, Rec1, element(Id2, Rec2))),
				     mnesia:write(remove_from(Index2, Rec2, element(Id1, Rec1)))
			    end,
			transaction(F);
	_ -> false
    end.

%%%%%%%%%%%%%%%%%%%% internal utility
extract_fields(Rec, Fields) -> list_to_tuple(lists:map(fun (F) -> element(F, Rec) end, Fields)).
