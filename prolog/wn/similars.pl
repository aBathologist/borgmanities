:- module(similars,[similars/2]).

:- use_module(db).

src('db/wn_sim.pl').

load :-
    src(Src),
    ensure_loaded(Src).

similars(IdA, IdB) :-
    src(Src),
    call_ensuring_src_loaded(sim(IdA, IdB), Src).
