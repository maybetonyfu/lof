:- module(mymod, []).
:- multifile pet/1, food/1.

mymod:pet(dog).
mymod:food(carrot).
animal(X) :- mymod:pet(X).

:- export(animal/1).
:- export(pet/1).
:- export(food/1).