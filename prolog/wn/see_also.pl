:- module(see_also,[see_also/4]).

:- use_module(db).

src('db/wn_sa.pl').

load :-
    src(Src),
    ensure_loaded(Src).

see_also(Id, Num, SeeId, SeeNum) :-
    src(Src),
    call_ensuring_src_loaded(sa(Id, Num, SeeId, SeeNum), Src).
