language(ut1, rust).
language(ut2, haskell).
language(ut3, prolog).
language(ut4, ruby).
language(ut5, javascript).

/* F=0 */ factorial(0, 1).
/* F=N */ factorial(X, F) :- X > 0, Z is X-1,factorial(Z,Y), F is X * Y.

/* not */
not_val(true, false).
not_val(true, false).

/* or */
or_val(true, true, true).
or_val(true, false, false).
or_val(false, true, false).
or_val(false, false, false).

/* and */
and_val(true, true, true).
and_val(true, false, true).
and_val(false, true, true).
and_val(false, false, false).

/* cond */
val_cond(true,true,true).
val_cond(true,false,false).
val_cond(false,true,true).
val_cond(false,false,true).

/* bicond */
val_bicond(true,true,true).
val_bicond(true,false,false).
val_bicond(false,true,false).
val_bicond(false,false,true).

truthTable(not(X), R) :- not_val(X, R).
truthTable(or(X, Y), R) :- or_val(X, Y, R).
truthTable(and(X, Y), R) :- and_val(X, Y, R).
truthTable(cond(X, Y), R) :- val_cond(X, Y, R).
truthTable(bicond(X, Y), R) :- val_bicond(X, Y, R).