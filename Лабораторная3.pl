
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
