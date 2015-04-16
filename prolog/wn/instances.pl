:- module(instances,[rel_instances/2]).

:- use_module(db).

src(wn(db/wn_ins)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_instances(IdA, IdB) :-
    src(Src),
    call_ensuring_src_loaded(ins(IdA, IdB), Src).
