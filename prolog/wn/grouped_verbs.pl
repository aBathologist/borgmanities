:- module(grouped_verbs,[rel_grouped_verbs/4]).

:- use_module(db).

src(wn(db/wn_vgp)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_grouped_verbs(IdA, WordNumA, IdB, WordNumB) :-
    src(Src),
    call_ensuring_src_loaded(vgp(IdA, WordNumA, IdB, WordNumB), Src).

