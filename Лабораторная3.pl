
readList(0,[]) :- !.
readList(I,[X|T]) :- write('input - '),read(X), I1 is I - 1, readList(I1, T).

write_list([]) :- !.
write_list([X|T]) :- write(X), nl, write_list(T).

concatL([], List2, List2).
concatL([H|T],List2,[H|NewList]) :- concatL(T,List2,NewList).

length14([],0):-!.
length14([_|T], CNTS) :- length(T,I), CNTS is I + 1.
%11. chet, nechet
chetnechet(L):-length14(L,Len),chetnechet(L,0,Len,[],[]).
chetnechet([],F,F,A,B):-append(A,B,C),write_list(C).
chetnechet([H|T],In,Le,Ch,Nch):-
    In mod 2 =:= 0,
    append(Ch,[H],Ch1),
    In1 is In + 1,
    chetnechet(T,In1,Le,Ch1,Nch),!;
    append(Nch,[H],Nch1),
    In2 is In +1,
    chetnechet(T,In2,Le,Ch,Nch1),!.


task11:-
    write('Input N -'),
    read(N),
    readList(N,L),
    chetnechet(L).

%12.  [a;b]. sum elem >a <b

sumElem([H|T],A,B,S):-sumElem([H|T],A,B,S,0).
sumElem([],_,_,S,S):-!.
sumElem([H|T],A,B,S,Sum):-
    H>=A , B>=H ,Sum1 is Sum+H,!,sumElem(T,A,B,S,Sum1);
    sumElem(T,A,B,S,Sum),!.

task12:-
    write('Input N -'),
    read(N),
    readList(N,L),
    write('Input A -'),read(A), write('Input B -'),read(B),
    sumElem(L,A,B,S),write(S).

%13. ƒл€ введенного списка построить два списка L1 и L2, где элементы L1
% это неповтор€ющиес€ элементы исходного списка, а элемент списка L2 с
% ноmером i показывает, сколько раз элемент списка L1 с таким номером
%повтор€етс€ в исходном.
prov([],_):-!.
prov([H|T],A):-
    H=:=A,!,fail.
prov([H|T],A):-
    prov(T,A).

kolvo([],_,K,K):-!.
kolvo([H|T],A,K):-kolvo([H|T],A,K,0).
kolvo([H|T],A,K,C):-
    H=:=A,K1 is C+1,kolvo(T,A,K,K1).
kolvo([H|T],A,K,C):-
    kolvo(T,A,K,C).

strL1(L,N):-strL1(L,[],N).
strL1([],K,K):-!.
strL1([H|T],N,K):-
    prov(N,H),concatL(N,[H],N1),
    strL1(T,N1,K).
strL1([H|T],N,K):-strL1(T,N,K).


strL2(L,L1,N):-strL2(L,L1,[],N).
strL2([],_,K,K):-!.
strL2([H|T],L1,N,K):-
    kolvo(L1,H,M),concatL(N,[M],N1),
    strL2(T,L1,N1,K).

task13:-
    write('Input N -'),
    read(N),
    readList(N,L),
    strL1(L,M),
    strL2(M,L,K),
    write('L1:'),
    write_list(M),
    write('L2:'),
    write_list(K).
