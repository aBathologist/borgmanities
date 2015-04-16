:- module(sense_keys,[rel_sense_keys/3]).

:- use_module(db).

src(wn(db/wn_sk)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_sense_keys(Id, Num, SenseKey) :-
    src(Src),
    call_ensuring_src_loaded(sk(Id, Num, SenseKey), Src).
