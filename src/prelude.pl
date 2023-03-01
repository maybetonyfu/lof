type_of_length(T, _) :- T = function(list(X), int).
type_of_map(T, _) :- T = function(function(A, B), function(list(A), list(B))).
type_of_id(T, _) :- T = function(A, A).
type_of_head(T, _) :- T = function(list(X), X).
type_of_tail(T, _) :- T = function(list(X), list(X)).