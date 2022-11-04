
% Ejercicio 2.1

zip([], _, []).
zip([_|_], [], []).
zip([X|LIST1], [Y|LIST2], [pair(X,Y)|RT]) :- zip(LIST1, LIST2, RT).

unzip([], [], []).
unzip([pair(X,Y)|ZT], [X|XT], [Y|YT]) :- unzip(ZT, XT, YT).

% Ejercicio 2.2

treeH(nil,0).
treeH(tree(_,L,R),H):- treeH(L,Hl),treeH(R,Hr),H is max(Hl,Hr) +1.

% Ejercicio 2.3

start(tictactoe(empty,empty,empty, empty,empty,empty, empty,empty,empty)).

winer(tictactoe(A,B,C, D,E,F, G,H,I), P) :-
  % Rows
  (A = P, B = P, C = P; 
  D = P, F = P, G = P;
  G = P, H = P, I = P;
  % Cols
  A = P, D = P, G = P; 
  B = P, E = P, H = P;
  C = P, F = P, I = P;
  % Slash
  A = P, E = P, I = P;
  C = P, E = P, G = P), P \= empty.

