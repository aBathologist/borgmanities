:- module(participles,[participles/4]).

:- use_module(db).

src('db/wn_ppl.pl').

load :-
    src(Src),
    ensure_loaded(Src).

participles(AdjPplId, AdjPplNum, VerbId, VerbNum) :-
    src(Src),
    call_ensuring_src_loaded(ppl(AdjPplId, AdjPplNum, VerbId, VerbNum), Src).
