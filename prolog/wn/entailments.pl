:- module(entailments,[entailments/2]).

:- use_module(db).

src('db/wn_ent.pl').

load :-
    src(Src),
    ensure_loaded(Src).

entailments(EntailsId, EntailedId) :-
    src(Src),
    call_ensuring_src_loaded(ent(EntailsId, EntailedId), Src).
