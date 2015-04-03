:- module(par_meronyms,[par_meronyms/2]).

:- use_module(db).

src('db/wn_mp.pl').

load :-
    src(Src),
    ensure_loaded(Src).

par_meronyms(HolonymId, MeronymId) :-
    src(Src),
    call_ensuring_src_loaded(mp(HolonymId, MeronymId), Src).
