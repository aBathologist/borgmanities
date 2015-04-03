:- module(adj_syntax,[adj_syntax/3]).

:- use_module(db).

src('db/wn_syntax.pl').

load :-
    src(Src),
    ensure_loaded(Src).

adj_syntax(Id, Num, Syntax) :-
    src(Src),
    call_ensuring_src_loaded(syntax(Id, Num, Abbr), Src),
    abbreviation_syntax(Abbr, Syntax).

% "An adjective may be annotated with a syntactic marker indicating a limitation on the syntactic position the adjective may have in relation to noun that it modifies"

abbreviation_syntax(p,  predicate).
abbreviation_syntax(a,  prenominal).
abbreviation_syntax(ip, postnominal).
