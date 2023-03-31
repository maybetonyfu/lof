:- module(test2, [greeting/1]).
:- reexport('test').

greeting(X):- hello(X).

