:- use_module(library(assoc)).

not(true, false).
not(false, true).

and(true,true,true).
and(true,false,false).
and(false,true,false).
and(false,false,false).

or(true,true,true).
or(true,false,true).
or(false,true,true).
or(false,false,false).

cond(true,true,true).
cond(true,false,false).
cond(false,true,true).
cond(false,false,true).

bicond(true,true,true).
bicond(true,false,false).
bicond(false,true,false).
bicond(false,false,true).

truthTable(not(X),Y) :- not(X, Y).
truthTable(and(X,Z),Y) :- and(X, Z, Y).
truthTable(or(X,Z),Y) :- and(X, Z, Y).
truthTable(cond(X,Z),Y) :- and(X, Z, Y).
truthTable(bicond(X,Z),Y) :- and(X, Z, Y).

assign(A) :- list_to_assoc(["t" - true, "f" - false], A).

propEval(true, _, true).
propEval(false, _, false).
propEval(not(P), A, R) :- propEval(P, A, R2), not(R2, R).
propEval(and(P1, P2), A, R) :- propEval(P1, A, R1),  propEval(P2, A, R2), and(R1, R2, R).
propEval(or(P1, P2), A, R) :- propEval(P1, A, R1),  propEval(P2, A, R2), or(R1, R2, R).
propEval(cond(P1, P2), A, R) :- propEval(P1, A, R1),  propEval(P2, A, R2), cond(R1, R2, R).
propEval(bicond(P1, P2), A, R) :- propEval(P1, A, R1),  propEval(P2, A, R2), bicond(R1, R2, R).


propEval(propVar(V), A, R) :- get_assoc(V, A, R).