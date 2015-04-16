:- module(classifications,[rel_classifications/5]).

:- use_module(db).

src(wn(db/wn_cls)).

load :-
    src(Src),
    ensure_loaded(Src).

rel_classifications(MemberId, MemberNum, ClassId, ClassNum, ClassType) :-
    src(Src),
    call_ensuring_src_loaded(cls(MemberId, MemberNum, ClassId, ClassNum, AbbreviatedType), Src),
    abbreviation_class_type(AbbreviatedType, ClassType).

abbreviation_class_type(t, topical).
abbreviation_class_type(u, usage).
abbreviation_class_type(r, regional).
