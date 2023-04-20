:- module(mymod2, []).
:- use_module(library(yall)).


pet(cat).
mod:food(banana).

:- findall(PredicateIndicator, current_module_predicate(_, PredicateIndicator), Predicates),
   maplist(([P]>>(functor(P, Name, Arity), write(Name/Arity), nl, export(Name/Arity))), Predicates).