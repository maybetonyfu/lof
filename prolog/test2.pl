:- module(mymod2, []).
:- reexport('test.pl').
:- use_module('test.pl').

mymod:pet(cat).
mymod:food(banana).

