:- module(antonyms,[rel_antonyms/4]).

:- use_module(db).

src(wn(db/wn_ant)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_antonyms(Id, WordNum, AntonymId, AntonymNum) :-
    src(Src),
    call_ensuring_src_loaded(ant(Id, WordNum, AntonymId, AntonymNum), Src).
