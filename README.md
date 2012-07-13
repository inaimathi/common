# common
*Generally useful helper Erlang helper functions*

### db
*Just basics that gloss over annoying bits of the `mnesia` api. Mostly taken from the examples in [Programming Erlang](http://pragprog.com/book/jaerlang/programming-erlang), and not my own creations*

    do/1 (qlc:q term) -> transaction result
    
Takes a `qlc:q` expression, and wraps it in a `transaction`+`fun`+`qlc:e`. Returns the result of the query rather than `{atomic, Result}` (fails on non-atomic results).

    transaction/1 (fun) -> transaction result

Takes a `fun` and wraps it in a `mnesia:transaction/1`, returning `Result` rather than `{atomic, Result}`. Fails on non-atomic results. Called in `db:do/1`.

    atomic_insert/1 (Record || [Records]) -> result of atomic write
    
Takes record or list of records, and returns the result of doing an atomic insertion on the input. If a list of records is provided, `map` is inside the transaction, so they will either all be inserted or none of them will be.

    push_to/3 (Index, Rec, NewVal) -> modified Rec

Takes a record index, a record and a value. Returns `Rec` with `NewVal` pushed into `Index`.

    remove_from/3 (Index, Rec, Prey) -> modified Rec
    
Takes a record index, a record and a value. Returns `Rec` with the first instance of `Prey` removed from `Rec`.

    relate/6 (Rec1, Id1, Index1, Rec2, Id2, Index2) -> relates records

Takes two records, and their corresponding IDs + ID indexes. Atomically relates the two records in `mnesia`. This and `decouple/6` are used in managing many-to-many relationships (this one mutually adds the appropriate `Id` to the appropriate `Index`). Note that these do not check for existence, which should probably be changed before long.

    decouple/6 (Rec1, Id1, Index1, Rec2, Id2, Index2) -> decouples records
    
Takes two records, and their corresponding IDs + ID indexes. Atomically decuples the two records in `mnesia`. This and `relate/6` are used in managing many-to-many relationships (this one mutually removes the appropriate `Id` from the appropriate `Index`). Note that these do not check for existence, which should probably be changed before long.

### common
*Simple functions that I use in lots of places. I'm not entirely sure why they're not in the standard libraries.*

    now_to_seconds/1 ({A, B, C}) -> SecondsFromEpoch
    
Takes a `now()` and converts it to a number of seconds. Useful in time/duration comparisons.

    binary_to_hex/1 (<<Binary>>) -> HexString
    
Takes a binary and returns its hex string representation. Useful when displaying the results of hash/encryption functions.

    make_tempname/0 () -> TempName
    make_tempname/1 (TargetDir) -> TempName
    
Returns a unique temporary pathname. Doesn't use `os:cmd("mktemp").` in an effort to stay kind of platform independent. The default directory is `/tmp` (wrapped with a `filename:nativename/1` call, so it does `\\tmp` on Windows). Optionally takes a target directory.

### gpg
*basic, command-line bindings to GPG. Don't use this, it's still very raw.*
