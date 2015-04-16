:- module(derivations,[rel_derivations/4]).

:- use_module(db).

src(wn(db/wn_der)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_derivations(IdA, WordNumA, IdB, WordNumB) :-
    src(Src),
    call_ensuring_src_loaded(der(IdA, WordNumA, IdB, WordNumB), Src).
