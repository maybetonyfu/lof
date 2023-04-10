:- module(mymod, [pet/1, food/1, animal/1]).
:- multifile pet/1, food/1.

mymod:pet(dog).
mymod:food(carrot).



animal(X) :- mymod:pet(X).