type_of_SYMcn(T, _) :-
  '='(FreshB22, T),
  '='(FreshB23, _a),
  '='(FreshB25, adt('[|]'(list, '[|]'(FreshB27, nil)))),
  '='(FreshB27, _a),
  '='(FreshB26, adt('[|]'(list, '[|]'(FreshB28, nil)))),
  '='(FreshB28, _a),
  '='(FreshB24, adt('[|]'(function, '[|]'(FreshB25, '[|]'(FreshB26, nil))))),
  '='(FreshB22, adt('[|]'(function, '[|]'(FreshB23, '[|]'(FreshB24, nil))))).

type_of_undefined(T, _) :- true.