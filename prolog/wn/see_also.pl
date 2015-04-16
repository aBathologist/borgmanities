:- module(see_also,[rel_see_also/4]).

:- use_module(db).

src(wn(db/wn_sa)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_see_also(Id, Num, SeeId, SeeNum) :-
    src(Src),
    call_ensuring_src_loaded(sa(Id, Num, SeeId, SeeNum), Src).
