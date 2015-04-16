:- module(adj_syntax,[rel_adj_syntax/3]).

:- use_module(db).

src(wn(db/wn_syntax)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_adj_syntax(Id, Num, Syntax) :-
    src(Src),
    call_ensuring_src_loaded(syntax(Id, Num, Abbr), Src),
    abbreviation_syntax(Abbr, Syntax).

% "An adjective may be annotated with a syntactic marker indicating a limitation on the syntactic position the adjective may have in relation to noun that it modifies"

abbreviation_syntax(p,  predicate).
abbreviation_syntax(a,  prenominal).
abbreviation_syntax(ip, postnominal).
