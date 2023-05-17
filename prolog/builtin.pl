:- module(hs_builtin, [type_of_SYMcn/2,type_of__hsmd_Builtin_hsmd_error/2,member1/2]).

type_of_SYMcn(T, _) :-T = pair(function(a), pair(function(pair(list, a)), pair(list, a))).

type_of__hsmd_Builtin_hsmd_error(_, _).

member1(L,[L|_]) :- !.
member1(L,[_|RS]) :- member1(L,RS).