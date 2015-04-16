:- module(participles,[rel_participles/4]).

:- use_module(db).

src(wn(db/wn_ppl)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_participles(AdjPplId, AdjPplNum, VerbId, VerbNum) :-
    src(Src),
    call_ensuring_src_loaded(ppl(AdjPplId, AdjPplNum, VerbId, VerbNum), Src).
