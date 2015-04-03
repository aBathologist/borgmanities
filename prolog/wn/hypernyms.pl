:- module(hypernyms,[hypernyms/2]).

:- use_module(db).

src('db/wn_hyp.pl').

load :-
    src(Src),
    ensure_loaded(Src).

hypernyms(HyponymId, HypernymId) :-
    src(Src),
    call_ensuring_src_loaded(hyp(HyponymId, HypernymId), Src).
