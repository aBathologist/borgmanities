:- module(frames,[rel_frames/3]).

:- use_module(db).

src(wn(db/wn_fr)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_frames(Id, FrameNum, WordNum) :-
    src(Src),
    call_ensuring_src_loaded(fr(Id, FrameNum, WordNum), Src).
