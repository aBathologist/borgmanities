:- module(causes,[rel_causes/2]).

:- use_module(db).

src(wn(db/wn_cs)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_causes(EffectId, CauseId) :-
    src(Src),
    call_ensuring_src_loaded(cs(EffectId, CauseId), Src).
