:- module( glosses,
           [ glosses/2 ]
         ).

:- use_module(db).

src('db/wn_g.pl').

load :-
    src(Src),
    ensure_loaded(Src).
    
glosses(Id, Glosses) :-
    src(Src),
    call_ensuring_src_loaded(g(Id, Glosses), Src).
