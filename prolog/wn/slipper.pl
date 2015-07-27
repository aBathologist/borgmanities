:- module(slipper,
          [ word_slips/2,
            word_randomSlip/2
          ]).

:- use_module(word).

word_slips(Word, Slips) :-
    word_entries(Word, Es),
    phrase(entries_slips(Es), Slips).

% Randomizes a list of all slips, and then picks a different member of
% on backtracking.
%
word_randomSlip(Word, Slip) :-
    word_slips(Word, Slips),
    random_permutation(Slips, RandomSlips),
    member(Slip, RandomSlips).

entry_slips(entry(_,_,Syn,Hyper,Hypo)) --> Syn, Hyper, Hypo.
entries_slips([]) --> [].
entries_slips([E|Es]) --> entry_slips(E), entries_slips(Es).
