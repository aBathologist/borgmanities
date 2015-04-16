:- module(par_meronyms,[rel_par_meronyms/2]).

:- use_module(db).

src(wn(db/wn_mp)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_par_meronyms(HolonymId, MeronymId) :-
    src(Src),
    call_ensuring_src_loaded(mp(HolonymId, MeronymId), Src).
