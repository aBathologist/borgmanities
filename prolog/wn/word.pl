%% :- module(word, []).
:- use_module(
           [
               db,
               synsets,
               glosses
           ]).

%% TBD:
%
%   - Update code in this module to use new modular interface to wnbd.
%   - Finish adding all all query attribute dcgs.
%   - Compare results with wordnet web query.
%   - Write tests for predciates.
%   - later:
%       - Implement as SWI-Prolog pack
% 

%   module word provides the man interface for constructing queries
%   to the Prolog version of the Wordnet Databse version 3.0
%   (henceforth, wndb).
%
%   It is being construted with the hope of proving a flexible interface
%   that can be used for any number of purposes. So there are any number
%   of predicates a user might find useful. However, for the purposes of
%   constructing a query and returning semantic and (some) syntactic information
%   from the wndb, the main interface predciates are word/2, word_fields/2, and word_fields/3:
%
%       word(?Word:atom, SynsetId:integer)
%       word_fields(?Word:atom, +FieldsToQuery:field_dcg_rules)
%       word_fields(?Word:atom, +FieldsToQuery:field_dcg_rules, ?QueryResults:list(?FieldName:atom-?FieldValue))



        /*****************************************
        *                                        *
        *       DATABASE AND RELATION NAMES      *
        *                                        *
        *****************************************/

% user:file_search_path(db, '/Users/aporiac/projects/borgmanities/prolog/db').

%% relation_wnOp(?RelationName, ?WnOperator)
%
%   Inteligible names for wordnet operators:

%% relation_wnOp(synsets,         s).
%% relation_wnOp(sense_keys,      sk).
%% relation_wnOp(glosses,         g).
%% relation_wnOp(syntax,          syntax).
%% relation_wnOp(hypernyms,       hyp).
%% relation_wnOp(instances,       ins).
%% relation_wnOp(entailments,     ent).
%% relation_wnOp(similars,        sim).
%% relation_wnOp(mem_meronyms,    mm).
%% relation_wnOp(sub_meronyms,    ms).
%% relation_wnOp(par_meronyms,    mp).
%% relation_wnOp(derivations,     der).
%% relation_wnOp(classifications, cls).
%% relation_wnOp(causes,          cs).
%% relation_wnOp(grouped_verbs,   vgp).
%% relation_wnOp(attributes,      at).
%% relation_wnOp(antonyms,        ant).
%% relation_wnOp(see_also,        sa).
%% relation_wnOp(participles,     ppl).
%% relation_wnOp(pertainym,       per).
%% relation_wnOp(frames,          f).


%% %% apply_relation(+Relation, ?Args)
%% %
%% %   Apply a wn operator to a list of args
%% %   using the relation name.

%% apply_rel(Relation, Args) :-
%%     relation_wnOp(Relation, Op),
%%     apply(Op, Args).

%% %% relation_file(?RelationName:atom, ?WnDBFileName:atom)
%% %
%% %   Get the names of wordnet db files using the relation
%% %   names

%% relation_file(Relation, File) :-
%%     relation_wnOp(Relation, Op),
%%     atom_concat(wn_, Op, File).

%% %% load_wn_database(+Database:atom)
%% %
%% %   Load a wordnet db using the the name of relation
%% %   defined in the databse.

%% load_wn_database(Database) :-
%%     relation_file(Database, File),
%%     consult(db(File)).


        /*****************************************
        *                                        *
        * DCGS FOR ASSEMBLING INFO ABOUT A WORD. *
        *                                        *
        *****************************************/


state(S0), [S0]     --> [S0].
state(S0, S1), [S1] --> [S0].

%% get_word(?Word:atom, ?Attrs:pairs)
%
%   bi-directional initiatior of the implicit
%   word DCG. The word is represented as a list of pairs,
%   the word itself given with the key `word`.
%
%   Get word can finda a basic list of attributes given
%   a `Word` or a `Word` given a list of attributes.
%   
get_word(Word), [Attrs] -->
    {
        apply_rel(synsets, [Id,W_Num,Word,Type,_,_]),
        Attrs = [word-Word, id-Id, num-W_Num, type-Type]
    }.

%% Accessing and setting attributes:
attr(Label-Attr) --> state(Attrs),
    { member(Label-Attr, Attrs) }.

set_attr(Label-Attr) --> state(Attrs, NewAttrs),
    {
        ( select(Label-_, Attrs, Label-Attr, NewAttrs), !
        ; NewAttrs = [Label-Attr|Attrs]
        )
    }.



        /*****************************************
        *                                        *
        *       ADDING FIELDS TO WORD QUERY      *
        *                                        *
        *****************************************/



%% GLOSS

gloss --> gloss(both).

gloss(Type) --> attr(id-Id),
                { apply_rel(glosses, [Id, Gloss]) },
                gloss(Type, Gloss).

gloss(both, Gloss) --> gloss(def, Gloss), gloss(ex, Gloss).
gloss(def, Gloss) --> set_attr(defs-Defs),
                      {
                          atomic_list_concat(Parts, '; ', Gloss),
                          exclude(prefex_of_atom('"'), Parts, Defs)
                      }.

gloss(ex,  Gloss) --> set_attr(exs-Exs),
                      {
                          atomic_list_concat(Parts, '; ', Gloss),
                          include(prefex_of_atom('"'), Parts, Exs)
                      }.

prefex_of_atom(Prefix, Atom) :-
    atom_prefix(Atom, Prefix).

%% HYPER- & HYPO-NYMS

hypernyms --> attr(word-Word), attr(id-Id),
              { word_ids_hypernyms(Word, Id, _, Hypernyms) },
              set_attr(hypernyms-Hypernyms).

hyponyms --> attr(word-Word), attr(id-Id),
             { word_ids_hypernyms(Word,_,Id, Hyponyms) },
             set_attr(hyponyms-Hyponyms).

%% TBD: Trace down SO caused by some looping here when
%   querried with `Word` free...
%

word_ids_hypernyms(Word, HypoId, HyperId, Hypernyms) :-
    findall( HyperId,
             apply_rel(hypernyms, [HypoId, HyperId]),
             HyperIds),
    findall( Hypernym,
             maplist(word_sid, Hypernym, HyperIds),
             NestedHypernyms),
    flatten(NestedHypernyms, ToExclude),
    exclude(=(Word), ToExclude, ToSort),
    ToSort \= [],
    sort(ToSort, Hypernyms).

%% INSTANCES

instances -->
    attr(id-Id),
    {
        findall(InsId, ins(InsId, Id), InsIds),
        InsIds \= [],
        maplist(synset_words, InsIds, NestedWords),
        flatten(NestedWords, Words)
    },
    set_attr(instances-Words), !.

%% ENTAILMENTS

entailments -->
    guard_attr_rel(
            attr(type-v),
            entailments
        ).

%% SIMILARS

similars -->
    guard_attr_rel(
            attr(type-a),
            similars
        ).

%% MERONYMS & HOLONYMS

% TBD: some of these should be using bagof or setof
% to gather all results ...

mem_holonyms -->
    guard_attr_rel(
            attr(type-n),
            mem_holonyms,
            mem_meronyms
        ).
mem_meronyms -->
    guard_attr_inv_rel(
            attr(type-n),
            mem_meronyms
        ).

sub_holonyms -->
    guard_attr_rel(
            attr(type-n),
            sub_holonyms,
            sub_meronyms
        ).
sub_meronyms -->
    guard_attr_inv_rel(
            attr(type-n),
            sub_meronyms
        ).

par_holonyms -->
    guard_attr_rel(
            attr(type-n),
            par_holonyms,
            par_meronyms
        ).
par_meronyms -->
    guard_attr_inv_rel(
            attr(type-n),
            par_meronyms
        ).


%% DERIVATIONS

derivations -->
    attr(id-Id), attr(num-N),
    {
        findall(DerId-Num,
                apply_rel(derivations, [Id, N, DerId, Num]),
                DerIdsNums),
        DerIdsNums \= [],
        pairs_keys(DerIdsNums, Ids),
        pairs_values(DerIdsNums, Nums),
        maplist(word_sid_num, Words, Ids, Nums)
    },
    set_attr(derivations-Words).

%% CLASSIFICATIONS

classifications -->
    attr(id-Id), attr(num-Num),
    {
        findall(ClassId-ClassNum,
                apply_rel(classifications,
                          [Id, Num, ClassId, ClassNum, _]),
                ClassIdsNumsTypes),
        ClassIdsNumsTypes \= [],
        pairs_keys(ClassIdsNumsTypes, Ids),
        pairs_values(ClassIdsNumsTypes, ClassNums),
        maplist(word_sid_num, Words, Ids, ClassNums)
    },
    set_attr(classifications-Words).    

%% TBD: Define guard_attr_rel_num that differentiates with
%% word numbers.

%% CAUSES
causes -->
    guard_attr_rel(
            attr(type-v),
            causes
        ).

%% ATTRIBUTES
attributes -->
    guard_attr_rel(
            (attr(type-a) ; attr(type-n)),
            attributes
        ).

%% GROUPED VERBS
grouped_verbs -->
    attr(type-v), attr(id-Id), attr(num-Num),
    {
        findall(VerbId-VerbNum,
                apply_rel(grouped_verbs,
                          [Id, Num, VerbId, VerbNum]),
               VerbIdNums),
        VerbIdNums \= [],
        pairs_keys(VerbIdNums, VerbIds),
        pairs_values(VerbIdNums, VerbNums),
        maplist(word_sid_num, Words, VerbIds, VerbNums)
    },
    set_attr(grouped_verbs-Words).

        /*****************************************
        *                                        *
        *       GENERALLY APPLICABLE DCGs:       *
        *                                        *
        *****************************************/

attr_rel(Attr) -->
    guard_attr_rel({true}, Attr, Attr).

%% guard_attr_rel(?Guard:dcgs, ?Attr:atom)
%
%   Is the same as guard_attr_rel(Guard, Attr, Attr)

guard_attr_rel(Guard, Attr) -->
    guard_attr_rel(Guard, Attr, Attr).
guard_attr_inv_rel(Guard, Attr) -->
    guard_attr_inv_rel(Guard, Attr, Attr).

%% guard_attr_rel(Guard, Label, Rel)
%
%   If `Guard` is true,
%   then add all words of synset with relation
%   `Rel` to implicit as an attribute `Label`.

guard_attr_rel(Guard, Label, Rel) -->
    Guard,
    related_words(Rel, Words),
    set_attr(Label-Words).
guard_attr_inv_rel(Guard, Label, Rel) -->
    Guard,
    inv_related_words(Rel, Words),
    set_attr(Label-Words).

%% All words of a synset that bear relation `Relation` to the
%% word in question.
related_words(Relation, Words) -->
    attr(id-Id),
    {
        findall( RelataId,
                 apply_rel(Relation, [Id, RelataId]),
                 RelataIds
               ),
        maplist(synset_words, RelataIds, NestedWords),
        flatten(NestedWords, Words),
        Words \= []
    }.
inv_related_words(Relation, Words) -->
    attr(id-Id),
    {
        findall( RelataId,
                 apply_rel(Relation, [RelataId, Id]),
                 RelataIds
               ),
        maplist(synset_words, RelataIds, NestedWords),
        flatten(NestedWords, Words),
        Words \= []
    }.


synset_words(ID, Words) :- findall(Word, word_sid(Word, ID), Words).

%% do dcg with state, unless dcg fails,
%% then just pass the state along unchanged.

maybe(DCG) --> DCG, !
             ; state(_). 


        /*****************************************
        *                                        *
        *     Predicates for using the dcgs:     *
        *                                        *
        *****************************************/

word(Word, Result) :-
    phrase(get_word(Word), [], [Result]).
word_fields(Word, Fields) :-
    word_fields(Word, Fields, _).
word_fields(Word, Fields, Result) :-
    phrase((get_word(Word), Fields), [], [Result]).

%% word_entry(Word, Entry) :-
%%     s(S_Id, W_Num, Word, Type, Sense_Number, Tag_Count),
%%     g(S_Id, Gloss).

%% init_word(Word, WordTerm) :-
%%     s(Id,Num,Word,Type,_,_),
%%     word(Id,Num,Word,Type).

%% get_word_attr(Word-Attrs, Attr, Value) :-
%%     member(Attr-Value, Attrs).
     
word_sid(Word, Sid) :-
    s(Sid,_,Word,_,_,_). 
word_sid_num(Word, Sid, Num) :-
    s(Sid,Num,Word,_,_,_).
word_type(Word, Type) :-
    s(_,_,Word,Type,_,_).

%% word_gloss(Word, Sid, Gloss) :-
%%     word_sid_num(Word, Sid, _),
%%     word_sid_num(Word, )

word_sid_hypernym(Word, Wsid, Hypernym, Hsid) :-
    (ground(Word), var(Wsid) -> word_sid_num(Word, Wsid, _) ; true),
    hyp(Wsid, Hsid),
    word_sid_num(Hypernym, Hsid, _).

all_hypo_and_hypernyms(Word, Nyms) :-
    findall(
            Nym,
            ( hyponym_hypernym(Word, Nym)
            ; hyponym_hypernym(Nym, Word) ),
            Nyms
        ).









