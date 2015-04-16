:- module( glosses,
           [ rel_glosses/2 ]
         ).

:- use_module(db).

src(wn(db/wn_g)).

load :-
    src(Src),
    ensure_loaded(Src).
    
rel_glosses(Id, Glosses) :-
    src(Src),
    call_ensuring_src_loaded(g(Id, Glosses), Src).
