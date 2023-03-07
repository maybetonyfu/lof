type_of_length(T, _) :- T = function(list(X), int).
type_of_map(T, _) :- T = function(function(A, B), function(list(A), list(B))).
type_of_id(T, _) :- T = function(A, A).
type_of_head(T, _) :- T = function(list(A), A).
type_of_tail(T, _) :- T = function(list(A), list(A)).
type_of_SYMcn(T, _) :- T = function(A, function(list(A), list(A))).
type_of_SYMpl(T, _) :- T = function(A, function(A, A)), instance_of(A, num).
type_of_SYMmn(T, _) :- T = function(A, function(A, A)), instance_of(A, num).
type_of_SYMas(T, _) :- T = function(A, function(A, A)), instance_of(A, num).
type_of_not(T, _) :- T = function(bool, bool).
type_of_SYMeqSYMeq(T, _) :- T= function(A, function(A, bool)).
type_of_SYMslSYMeq(T, _) :- T= function(A, function(A, bool)).
type_of_SYMplSYMpl(T, _) :- T = function(list(A), function(list(A), list(A))).
instance_of(int, num).
instance_of(float, num).
type_of_Nothing(T, _) :- T = adt(maybe, A).
type_of_Just(T, _) :- T = function(A, adt(maybe, A)).