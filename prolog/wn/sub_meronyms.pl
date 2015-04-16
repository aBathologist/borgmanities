:- module(sub_meronyms,[rel_sub_meronyms/2]).

:- use_module(db).


% "sub" is short for "substance"

src(wn(db/wn_ms)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_sub_meronyms(HolonymId, MeronymId) :-
    src(Src),
    call_ensuring_src_loaded(ms(HolonymId, MeronymId), Src).
