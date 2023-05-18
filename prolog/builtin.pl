:- module(hs_builtin, [type_of_SYMcn/2,type_of__hsmd_Builtin_hsmd_error/2,member1/2, alldif/1]).

type_of_SYMcn(T, _) :-
    T = pair(function(A), B),
    B = pair(function(C), D),
    C = pair(list, A),
    D = pair(list, A).

type_of__hsmd_Builtin_hsmd_error(_, _).

member1(L,[L|_]) :- !.
member1(L,[_|RS]) :- member1(L,RS).


alldif([]).
alldif([E|Es]) :-
   maplist(dif(E), Es),
   alldif(Es).