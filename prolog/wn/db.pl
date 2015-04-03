:- module(db,
          [ call_ensuring_src_loaded/2,
            relation_wnOp/2
          ]).

%   Provides predicates for managing efficient, easy loading
%   of wndbs. Using call_ensuring_src_loaded/2 allows relation
%   modules to be imported freely, since their rather large
%   source files will only be loaded if they are actually called
%   upon.

        /*****************************************
        *                                        *
        *           JIT DATABASE LOADING         *
        *                                        *
        *****************************************/

:- retractall(src_loaded(_)).

:- meta_predicate call_ensuring_src_loaded(0, :).
call_ensuring_src_loaded(P, Src) :-
    ( src_loaded(Src)
     -> call(P)
     ;  ensure_loaded(Src),
        assertz(src_loaded(Src)),
        call(P)
    ).

        /*****************************************
        *                                        *
        *       DATABASE AND RELATION NAMES      *
        *                                        *
        *****************************************/

%% relation_wnOp(?RelationName, ?WnOperator)
%
%   Inteligible names for wordnet operators:

relation_wnOp(synsets,         s).
relation_wnOp(sense_keys,      sk).
relation_wnOp(glosses,         g).
relation_wnOp(adj_syntax,      syntax).
relation_wnOp(hypernyms,       hyp).
relation_wnOp(instances,       ins).
relation_wnOp(entailments,     ent).
relation_wnOp(similars,        sim).
relation_wnOp(mem_meronyms,    mm).
relation_wnOp(sub_meronyms,    ms).
relation_wnOp(par_meronyms,    mp).
relation_wnOp(derivations,     der).
relation_wnOp(classifications, cls).
relation_wnOp(causes,          cs).
relation_wnOp(grouped_verbs,   vgp).
relation_wnOp(attributes,      at).
relation_wnOp(antonyms,        ant).
relation_wnOp(see_also,        sa).
relation_wnOp(participles,     ppl).
relation_wnOp(pertainym,       per).
relation_wnOp(frames,          fr).

%% relation_file(?RelationName:atom, ?WnDBFileName:atom)
%
%   Get the names of wordnet db files using the relation
%   names

relation_file(Relation, File) :-
    relation_wnOp(Relation, Op),
    atomic_list_concat([wn_, Op, '.pl'], File).

%% used to generate skeleton module interfaces for each wndb:
make_module(Rel) :-
    relation_wnOp(Rel, Op),
    relation_file(Rel, RelFile),
    atomic_list_concat([Rel, '.pl'], ModuleFile),
    Code = [':- module(',Rel,',[',Rel, '/<N>]).~n~n:- use_module(db).~n~nsrc(\'db/',RelFile,'\').~n~nload :-~n    src(Src),~n    ensure_loaded(Src).~n~n',Rel,'(<ARGS>) :-~n    src(Src),~n    call_ensuring_src_loaded(',Op,'(<ARGS>), Src).'],
    atomic_list_concat(Code, CodeAtom),
    open(ModuleFile, write, Stream),
    format(Stream, CodeAtom, []),
    close(Stream).
