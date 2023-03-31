type_of_even(T, Callers) :-
    member(even, Callers), !,
    T = int.

type_of_even(T, Callers) :-
    T = int,
    type_of_odd(T, [even|Callers]).

type_of_odd(T, Callers) :-
    member(odd, Callers), !,
    T = int.
type_of_odd(T, Callers) :-
    T = int,
    type_of_even(T, [odd|Callers]).
