:- module(synsets,[synsets/6]).

:- use_module(db).

src('db/wn_s.pl').

load :-
    src(Src),
    ensure_loaded(Src).

synsets(SynsetId, WordNum, WordAtom, SynsetType, SenseNumber, TagCount) :-
    src(Src),
    call_ensuring_src_loaded(s(SynsetId, WordNum, WordAtom, SynsetType, SenseNumber, TagCount), Src).
