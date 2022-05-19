%11

max(X,Y,X):-X>Y,!.
max(_,Y,Y).

pr(1):- fail.
pr(X):-pr(X,2).
pr(X,X):-!.
pr(X,I):- 0 is X mod I,!,fail.
pr(X,I):- I1 is I+1, pr(X,I1).

maxprostU(X,Y):-maxprostU(X,X,Y),!.
maxprostU(X,N,N):-
    0 is X mod N,
    pr(N),!.
maxprostU(X,N,Y):-
    N1 is N-1,
    maxprostU(X,N1,Y).

max_pr_d(N,X):-m(N,N,X,2).
m(_,2,X,X):-!.
m(N,Cur,X,M):-
    pr(Cur),
    0 is N mod Cur,!,
    max(M,Cur,L),
    N1 is Cur-1,
    m(N,N1,X,L).
m(N,Cur,X,M):-
    NewCur is Cur - 1,
    m(N,NewCur,X,M).

%12 NOD max nechetnogo neprostogo delitelya i proizvedeniya.
nod(X,0,X):-!.
nod(X,Y,Z):- C is X mod Y,
    nod(Y,C,Z).

proizv(X,Y):-proizv(X,Y,1).
proizv(0,Y,Y):-!.
proizv(X,Y,Z):-
    N is X div 10,
    K is X mod 10,
    P is (Z*K),
    proizv(N,Y,P).

maxdel(X,Y):-maxdel(X,X,Y),!.
maxdel(X,Y,Y):-
    0 is X mod Y,
    not(pr(Y)),!.
maxdel(X,Y,Z):-
    Y1 is Y-1,
    maxdel(X,Y1,Z).

task_12(X,Y):-
    proizv(X,K),
    maxdel(X,Z),
    nod(K,Z,Y).
