:- module(sense_keys,[sense_keys/3]).

:- use_module(db).

src('db/wn_sk.pl').

load :-
    src(Src),
    ensure_loaded(Src).

sense_keys(Id, Num, SenseKey) :-
    src(Src),
    call_ensuring_src_loaded(sk(Id, Num, SenseKey), Src).
