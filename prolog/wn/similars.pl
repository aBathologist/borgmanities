:- module(similars,[rel_similars/2]).

:- use_module(db).

src(wn(db/wn_sim)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_similars(IdA, IdB) :-
    src(Src),
    call_ensuring_src_loaded(sim(IdA, IdB), Src).
