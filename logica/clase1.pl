language(ut1, rust).
language(ut2, haskell).
language(ut3, prolog).
language(ut4, ruby).
language(ut5, javascript).

/* F=0 */ factorial(0, 1).
/* F=N */ factorial(X, F) :- X > 0, Z is X-1,factorial(Z,Y), F is X * Y.