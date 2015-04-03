:- module(mem_meronyms,[mem_meronyms/2]).

:- use_module(db).

src('db/wn_mm.pl').

load :-
    src(Src),
    ensure_loaded(Src).

mem_meronyms(HolonymId, MeronymId) :-
    src(Src),
    call_ensuring_src_loaded(mm(HolonymId, MeronymId), Src).
