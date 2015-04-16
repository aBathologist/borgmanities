:- module(entailments,[rel_entailments/2]).

:- use_module(db).

src(wn(db/wn_ent)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_entailments(EntailsId, EntailedId) :-
    src(Src),
    call_ensuring_src_loaded(ent(EntailsId, EntailedId), Src).
