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
%13

divBy2(X,R) :-
    Mod is X mod 2,
    Val is X div 2,
    (0 is Mod,divBy2(Val,R);R is X).
divBy5(X,R) :-
    Mod is X mod 5,
    Val is X div 5,
    (0 is Mod,divBy5(Val,R);R is X).
numForPeriod(X,R) :- divBy2(X,R1),divBy5(R1,R2),R is R2.

period(D,R) :- numForPeriod(D,Res),period(Res,R,1),!.
period(D,R,LR) :-
    B10 is 10**LR,
    1 is B10 mod D,
    R is LR,!.
period(D,R,LR) :-
    B10 is 10**LR,
    0 is B10 mod D,
    R is 0,!.
period(D,R,LR) :-
    NewLR is LR + 1,
    period(D,R, NewLR).

findMaxPeriod(D) :- findMaxPeriod(D,2,0,2).
findMaxPeriod(D,1000,_,LocalIndex) :- D is LocalIndex,!.
findMaxPeriod(D,Index,LocalD, LocalIndex) :-
    period(Index,NextD),
    NewLocalD is max(NextD,LocalD),
    (NewLocalD>LocalD,NewLocalIndex is Index; NewLocalIndex is LocalIndex),
    NewIndex is Index + 1,
    findMaxPeriod(D,NewIndex, NewLocalD, NewLocalIndex),!.
% 14
% predicat: return list length

length14([],0):-!.
length14([_|T], CNTS) :- length(T,I), CNTS is I + 1.
%15.
readList(0,[]) :- !.
readList(I,[X|T]) :- write('input - '),read(X), I1 is I - 1, readList(I1, T).

write_list([]) :- !.
write_list([X|T]) :- write(X), nl, write_list(T).

%15(3).
elbyindex(L,I,El):-elbyindex(L,I,El,0).
elbyindex([H|_],K,H,K):-!.
elbyindex([_|Tail],I,El,Cou):-
    I =:= Cou,
    elbyindex(Tail,Cou,El,Cou);
    Cou1 is Cou + 1,
    elbyindex(Tail,I,El,Cou1).

maxElem(L,El):- maxElem(L,-1000,El).
maxElem([],El,El):-!.
maxElem([H|T],M,El):-
    (H>M,M1 is H),
    maxElem(T,M1,El);
    maxElem(T,M,El).

task15:-
    write('input N'),
    read(N),
    readList(N,L),
    read(I),
    elbyindex(L,I,Elind),
    maxElem(L,Elmax),
    (Elind =:= Elmax,write(yes);write(no)),!.
