:- module(attributes,[attributes/2]).

:- use_module(db).

src('db/wn_at.pl').

load :-
    src(Src),
    ensure_loaded(Src).

attributes(Id, AttrId) :-
    src(Src),
    call_ensuring_src_loaded(at(Id, AttrId), Src).
