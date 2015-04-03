:- module(instances,[instances/2]).

:- use_module(db).

src('db/wn_ins.pl').

load :-
    src(Src),
    ensure_loaded(Src).

instances(IdA, IdB) :-
    src(Src),
    call_ensuring_src_loaded(ins(IdA, IdB), Src).
