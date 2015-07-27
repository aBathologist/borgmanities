:- module(word,
          [ word_JSON/2,
            word_entry/2,
            word_entries/2,
            is_noun/1,
            is_adjective/1,
            is_verb/1,
            is_adverb/1 ]).

:- use_module([ wn, library(djson) ]).

/*
JSON SERIALIZATION OF RESULTS
*/

:- multifile djson:json//1.
djson:json(entry(Definition, PartOfSpeech, Synonyms, Hypernyms, Hyponyms)) -->
    json({definitions:Definition, partOfSpeech:PartOfSpeech,
          synonyms:Synonyms, hypernyms:Hypernyms, hyponyms:Hyponyms}).

djson:json(word_entries(Word, Entries)) -->
    json({word : Word, entries : Entries}).

word_JSON(Word, Results) :-
    word_entries(Word, Entries),
    json_term(Results, word_entries(Word, Entries)).

/*
QUERY PREDICATS
*/

word_id(Word, Id) :- word_id_num(Word, Id, _).
word_id_num(Word, Id, Num) :-
    rel_synsets(Id, Num, Word, _, _, _).

word_entry(Word, Entry) :-
    Entry = entry(Definitions, PartOfSpeech, Synonyms, Hypernyms, Hyponyms),
    word_id(Word, Id),
    id_partOfSpeech(Id, PartOfSpeech),
    id_definitions(Id, Definitions),
    id_word_synonyms(Id, Word, Synonyms),
    id_word_hypernyms(Id, Word, Hypernyms),
    id_word_hyponyms(Id, Word, Hyponyms).

word_entries(Word, Entries) :-
    Entries are { E | word_entry(Word, E) }.
    
/*
SPECIALIZED QUERIES
*/

is_noun(Word) :-
    word_id(Word, Id),
    id_partOfSpeech(Id, noun), !.
is_adjective(Word) :-
    word_id(Word, Id),
    id_partOfSpeech(Id, adjective), !.
is_verb(Word) :-
    word_id(Word, Id),
    id_partOfSpeech(Id, verb), !.
is_adverb(Word) :-
    word_id(Word, Id),
    id_partOfSpeech(Id, adverb), !.

/*
QUERYING ENTRY FIELDS GIVEN AN ID
*/

id_partOfSpeech(Id, PartOfSpeech) :-
    atom_number(IdAtom, Id),
    atom_begins(IdAtom, NumAtom),
    numAtom_partOfSpeech(NumAtom, PartOfSpeech).

    numAtom_partOfSpeech('1', noun).
    numAtom_partOfSpeech('2', verb).
    numAtom_partOfSpeech('3', adjective).
    numAtom_partOfSpeech('4', adverb).

id_definitions(Id, Definitions) :-
        rel_glosses(Id, Glosses),
        glosses_to_defs_examples(Glosses, Definitions, _).

    glosses_to_defs_examples(Glosses, Definitions, Examples) :-
        atomic_list_concat(Parts, '; ', Glosses),
        partition(is_example, Parts, Examples, Definitions).
    
    is_example(Atom) :-
        atom_begins(Atom, '"').

id_word_synonyms(Id, Word, Synonyms) :-
    %% word_id_num(Word, Id, Num),
    set(Synonyms) of { Synonym | word_id(Synonym, Id),
                                 Synonym \== Word}.

id_word_hypernyms(Id, Word, Hypernyms) :-
    set(Hypernyms) of { Hyper | rel_hypernyms(Id, HyperId),
                                word_id(Hyper, HyperId),
                                Hyper \== Word }.

id_word_hyponyms(Id, Word, Hyponyms) :-
    set(Hyponyms) of { Hypo | rel_hypernyms(HypoId, Id),
                              word_id(Hypo, HypoId),
                              Hypo \== Word }.

atom_begins(Atom, Begins) :-
    sub_atom(Atom, 0, 1, _, Begins).
