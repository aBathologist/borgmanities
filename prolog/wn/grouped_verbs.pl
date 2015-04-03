:- module(grouped_verbs,[grouped_verbs/4]).

:- use_module(db).

src('db/wn_vgp.pl').

load :-
    src(Src),
    ensure_loaded(Src).

grouped_verbs(IdA, WordNumA, IdB, WordNumB) :-
    src(Src),
    call_ensuring_src_loaded(vgp(IdA, WordNumA, IdB, WordNumB), Src).

