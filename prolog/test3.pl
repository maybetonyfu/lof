:- module(test3, [hail/1]).
:- reexport('test2').

hail(X):- greeting(X).
