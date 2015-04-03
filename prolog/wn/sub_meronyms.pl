:- module(sub_meronyms,[sub_meronyms/2]).

:- use_module(db).


% "sub" is short for "substance"

src('db/wn_ms.pl').

load :-
    src(Src),
    ensure_loaded(Src).

sub_meronyms(HolonymId, MeronymId) :-
    src(Src),
    call_ensuring_src_loaded(ms(HolonymId, MeronymId), Src).
