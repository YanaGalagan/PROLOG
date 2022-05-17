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
% 16(11) one of the elements is different from the others. find its
% value.
kolvo(L,N,Ot):-kolvo(L,N,Ot,0).
kolvo([],_,K,K):-!.
kolvo([H|T],El,Ot,K):-
    H =:= El, K1 is K+1,!,
    kolvo(T,El,Ot,K1);
    kolvo(T,El,Ot,K),!.


otl(L,Ot):-otl(L,L,Ot,0).
otl([],_,K,K):-!.
otl([H|T],L,Ot,C):-
    kolvo(L,H,K),
    K =:= 1,!,
    otl(T,L,Ot,H);
    otl(T,L,Ot,C),!.

task16:-
    write('input n-'),
    read(N),
    readList(N,L),
    otl(L,K),
    write(K).

%17(13).the elements located to the minimum should be placed at the end

indMinelem([H|T],Ot):-indMinelem(T,Ot,0,1,H).
indMinelem([],K,K,_,_):-!.
indMinelem([H|T],Ot,IM,I,Min):-
    (   H<Min,IM1 is I,Min1 is H;
    IM1 is IM,Min1 is Min),
    I1 is I+1,
    indMinelem(T,Ot,IM1,I1,Min1).

concatL([], List2, List2).
concatL([H|T],List2,[H|NewList]) :- concatL(T,List2,NewList).

moveBeforeMin([H|T],List):-indMinelem([H|T],Ind),
    moveBeforeMin([H|T],List,Ind,0,[]).
moveBeforeMin(L1,List,IndMin,IndMin,L2):- concatL(L1,L2,List),!.
moveBeforeMin([H|T],List,IndMin,IndNow,NowList):-
    NewInd is IndNow+1, concatL(NowList,[H],NewList),
    moveBeforeMin(T,List,IndMin,NewInd,NewList).

task17:-
    write('Input N -'),
    read(N),
    readList(N,List),
    moveBeforeMin(List,NewList),
    write('New List: '),
    write_list(NewList),!.

%local minimum by index

localMin(L,I):-I1 is I -1, I2 is I+1,elbyindex(L,I1,El1),
    elbyindex(L,I,El),elbyindex(L,I2,El2),
    El1>El,El2>El,!.

task18:-
    write('Input N -'),
    read(N),
    readList(N,L),
    write('Input I -'),
    read(I),
    localMin(L,I),!.


%cyclic shift of array elements to the left by one position.
appendL([],X,X).
appendL([X|T],Y,[X|T1]) :- appendL(T,Y,T1).


cyclicS(Result, 0, Result) :- !.
cyclicS([X|T], N, Result) :-
    N1 is N - 1,
    appendL(T, [X], NewList),
    cyclicS(NewList, N1, Result), !.

task19:-
    write('Input N -'),
    read(N),
    readList(N,L),
    cyclicS(L,1,K), write_list(K).

%local maximum by index

localMax(L,I):-I1 is I -1, I2 is I+1,elbyindex(L,I1,El1),
    elbyindex(L,I,El),elbyindex(L,I2,El2),
    El1<El,El2<El,!.

task20:-
    write('Input N -'),
    read(N),
    readList(N,L),
    write('Input I -'),
    read(I),
    localMax(L,I),!.

