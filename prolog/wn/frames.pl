:- module(frames,[frames/3]).

:- use_module(db).

src('db/wn_fr.pl').

load :-
    src(Src),
    ensure_loaded(Src).

frames(Id, FrameNum, WordNum) :-
    src(Src),
    call_ensuring_src_loaded(fr(Id, FrameNum, WordNum), Src).
