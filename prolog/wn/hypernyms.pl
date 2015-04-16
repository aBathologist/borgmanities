:- module(hypernyms,[rel_hypernyms/2]).

:- use_module(db).

src(wn(db/wn_hyp)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_hypernyms(HyponymId, HypernymId) :-
    src(Src),
    call_ensuring_src_loaded(hyp(HyponymId, HypernymId), Src).
