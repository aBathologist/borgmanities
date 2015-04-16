:- module(attributes,[rel_attributes/2]).

:- use_module(db).

src(wn(db/wn_at)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_attributes(Id, AttrId) :-
    src(Src),
    call_ensuring_src_loaded(at(Id, AttrId), Src).
