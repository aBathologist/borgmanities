:- module(causes,[causes/2]).

:- use_module(db).

src('db/wn_cs.pl').

load :-
    src(Src),
    ensure_loaded(Src).

causes(EffectId, CauseId) :-
    src(Src),
    call_ensuring_src_loaded(cs(EffectId, CauseId), Src).
