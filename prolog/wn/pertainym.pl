:- module(pertainym,[pertainym/4]).

% The per operator specifies two different relations based on the parts of speech involved. If the first word is in an adjective synset, that word pertains to either the noun or adjective second word. If the first word is in an adverb synset, that word is derived from the adjective second word.

:- use_module(db).

src('db/wn_per.pl').

load :-
    src(Src),
    ensure_loaded(Src).

pertainym(IdA, NumA, IdB, NumB) :-
    src(Src),
    call_ensuring_src_loaded(per(IdA, NumA, IdB, NumB), Src).
