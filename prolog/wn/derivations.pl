:- module(derivations,[derivations/2]).

:- use_module(db).

src('db/wn_der.pl').

load :-
    src(Src),
    ensure_loaded(Src).

derivations(IdA, IdB) :-
    src(Src),
    call_ensuring_src_loaded(der(IdA, IdB), Src).
