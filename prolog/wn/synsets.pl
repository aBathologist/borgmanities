:- module(synsets,[rel_synsets/6]).

:- use_module(db).

src(wn(db/wn_s)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_synsets(SynsetId, WordNum, WordAtom, SynsetType, SenseNumber, TagCount) :-
    src(Src),
    call_ensuring_src_loaded(s(SynsetId, WordNum, WordAtom, SynsetType, SenseNumber, TagCount), Src).
