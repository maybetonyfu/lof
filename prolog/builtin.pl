:- module(hs_builtin, [type_of_SYMcn/2,type_of__hsmd_Builtin_hsmd_error/2,member1/2]).


type_of_SYMcn(T, _) :-
  '='(FreshB22, T),
  '='(FreshB23, _a),
  '='(FreshB25, adt('[|]'(list, '[|]'(FreshB27, nil)))),
  '='(FreshB27, _a),
  '='(FreshB26, adt('[|]'(list, '[|]'(FreshB28, nil)))),
  '='(FreshB28, _a),
  '='(FreshB24, adt('[|]'(function, '[|]'(FreshB25, '[|]'(FreshB26, nil))))),
  '='(FreshB22, adt('[|]'(function, '[|]'(FreshB23, '[|]'(FreshB24, nil))))).

type_of__hsmd_Builtin_hsmd_error(_, _).

member1(L,[L|_]) :- !.
member1(L,[_|RS]) :- member1(L,RS).