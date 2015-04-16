:- module(mem_meronyms,[rel_mem_meronyms/2]).

:- use_module(db).

src(wn(db/wn_mm)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_mem_meronyms(HolonymId, MeronymId) :-
    src(Src),
    call_ensuring_src_loaded(mm(HolonymId, MeronymId), Src).
