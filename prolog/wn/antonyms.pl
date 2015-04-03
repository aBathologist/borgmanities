:- module(antonyms,[antonyms/4]).

:- use_module(db).

src('db/wn_ant.pl').

load :-
    src(Src),
    ensure_loaded(Src).

antonyms(Id, WordNum, AntonymId, AntonymNum) :-
    src(Src),
    call_ensuring_src_loaded(ant(Id, WordNum, AntonymId, AntonymNum), Src).
