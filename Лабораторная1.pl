man(voeneg).
man(ratibor).
man(boguslav).
man(velerad).
man(duhovlad).
man(svyatoslav).
man(dobrozhir).
man(bogomil).
man(zlatomir).

woman(goluba).
woman(lubomila).
woman(bratislava).
woman(veslava).
woman(zhdana).
woman(bozhedara).
woman(broneslava).
woman(veselina).
woman(zdislava).

parent(voeneg,ratibor).
parent(voeneg,bratislava).
parent(voeneg,velerad).
parent(voeneg,zhdana).

parent(goluba,ratibor).
parent(goluba,bratislava).
parent(goluba,velerad).
parent(goluba,zhdana).

parent(ratibor,svyatoslav).
parent(ratibor,dobrozhir).
parent(lubomila,svyatoslav).
parent(lubomila,dobrozhir).

parent(boguslav,bogomil).
parent(boguslav,bozhedara).
parent(bratislava,bogomil).
parent(bratislava,bozhedara).

parent(velerad,broneslava).
parent(velerad,veselina).
parent(veslava,broneslava).
parent(veslava,veselina).

parent(duhovlad,zdislava).
parent(duhovlad,zlatomir).
parent(zhdana,zdislava).
parent(zhdana,zlatomir).

men:-man(X),write(X),nl,fail.
women:-woman(X),write(X),nl,fail.

%11
doughter(X,Y):-parent(Y,X),woman(X).
doughter(X):-parent(X,Y),doughter(Y,X),print(Y),nl,fail.
%12
wife(X,Y):-woman(X),man(Y),parent(X,K),parent(Y,K),write(yes),!.
wife(X):-man(X),parent(X,Z),parent(Y,Z),woman(Y),write(Y),nl,!.
%13
grandMa(X,Y):-parent(X,Z),parent(Z,Y),woman(X),write(yes),nl,!.
grandMas(X):-parent(Z,X),parent(Y,Z),woman(Y),write(Y),nl.
%14
grand_ma_and_da(X,Y):-parent(X,Z),parent(Z,Y),woman(X),woman(Y),write(yes),nl,fail;parent(Y,Z),parent(Z,X),woman(X),woman(Y),write(yes),nl,fail.
%15
minch(0,9):-!.
minch(X,Y):-
     K is X // 10,
     N is X mod 10,
     minch(K,C),
     Y is min(C,N).
%16
minD(X,Y):-minD(X,Y,9).
minD(X,Y,Z):- X<10,Y is min(X,Z).
minD(X,Y,Z):-
    N is X // 10,
    K is X mod 10,
    Z1 is min(Z,K),
    minD(N,Y,Z1).
%17
prnot_5(0,1):-!.
prnot_5(N,Pr):-
    N1 is N div 10,
    Cifr is N mod 10,
    0 is Cifr mod 5,!,
    prnot_5(N1,Pr).
prnot_5(N,Pr):-
    N1 is N div 10,
    prnot_5(N1,Predpr),
    Cifr is N mod 10,
    Pr is Cifr*Predpr.
