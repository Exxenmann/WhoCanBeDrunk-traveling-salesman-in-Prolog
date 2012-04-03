% Author:
% Date: 23.03.2012

/*
p -> person
l -> location
b -> bar
f -> hat führerschein
se -> start / endpunkt
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%DEFINITIONEN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Personen
times(steve,1200,1800).
times(bill,1200,1800).
times(hans,1300,1700).
times(guido,1500,1730).
f(steve).
p(X) :- times(X,_,_).

%Bars
b(mata).
b(oblomow).

%Gruppierungen
se(X) :- p(X),f(X).
l(X) :- p(X).
l(X) :- b(X).

%Zeiten
t(1200).
t(1230).
t(1300).
t(1330).
t(1400).
t(1430).
t(1500).
t(1530).
t(1600).
t(1630).
t(1700).
t(1730).
t(1800).

%Wegenetz
cost(hans,bill,100).
cost(hans,steve,100).
cost(hans,mata,50).
cost(hans,oblomow,150).
cost(hans,guido,50).

cost(bill,steve,150).
cost(bill,mata,50).
cost(bill,oblomow,100).
cost(bill,guido,50).

cost(mata,steve,50).
cost(mata,oblomow,100).
cost(mata,guido,100).

cost(steve,oblomow,100).
cost(steve,guido,50).

cost(oblomow,guido,50).

get_cost(X,Y,Z) :- cost(X,Y,Z).
get_cost(X,Y,Z) :- cost(Y,X,Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Berechnung
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%MAIN

start_set(Out) :- setof(In,starts(In),Out).
starts([Startpunkt,Abfahrt,Ankunft,Ziellocation]) :-
                                             se(Startpunkt),
                                             p(Ziellocation),                                                 %erstes ziel muss Person sein! (alleine trinken macht keinen Spaß :) )
                                             dif(Startpunkt,Ziellocation),                                    %start und ziel müssen verschieden sein
                                             earliest_time(Startpunkt,Ziellocation,Abfahrt,Ankunft).                  %frühestmöglicher zeitpunkt

build_paths(All_paths) :- start_set(A),
                          persons_todo(A,Y),
                          length(Y,X),
                          X>0,
                          build_paths(
%build_paths(Start_move,Path) :-

/*
next_moves(Last_moves,Possible_next_moves) :- length(persons_todo(Last_moves,Persons_todo),X),
                                              X==0,
                                              add_bar(Last_moves, Possible_next_moves).*/

next_moves(Last_moves,Possible_next_moves) :- persons_todo(Last_moves,Y),
                                              length(Y,X),
                                              X>0,
                                              add_person(Last_moves,Possible_next_moves).

add_person(Last_moves,Possible_next_move) :- persons_todo(Last_moves,Persons_todo),
                                             member(NextPerson,Persons_todo),
                                              last_location(Last_moves,LL),
                                              actualTime(Last_moves,ActualTime),
                                              relative_earliest_time(LL,NextPerson,ActualTime,AbfahrtAlt,AnkunftNeu),
                                              append(Last_moves,[[LL,AbfahrtAlt,AnkunftNeu,NextPerson]],Possible_next_move).

last_location(Route,X) :- last(Route,Y),
                          last(Y,X).

% Hilfsfunktionen

relative_earliest_time(AlterOrt, NeuerOrt,ActualTime,AbfahrtAltesZiel,AnkunftGast) :- getstart(NeuerOrt, EarliestGuestStart),                         %frühester start bei Gast
                                                                                      get_cost(AlterOrt,NeuerOrt,Cost),                               %Kosten für Strecke
                                                                                      EarliestDriverArrival is ActualTime+Cost,                       %
                                                                                      greater_than(EarliestDriverArrival,EarliestGuestStart,AnkunftGast),
                                                                                      AbfahrtAltesZiel is AnkunftGast-Cost.
                                                                           

%rechnet abfahrt und ankunft für ersten schritt aus
earliest_time(Person1,Person2,Abfahrt,Ankunft) :- get_cost(Person1,Person2,Cost),
                                                  getstart(Person1,AbfahrtFahrer),
                                                  getstart(Person2,AnkunftFahrer),
                                                  Z1 is AbfahrtFahrer + Cost,
                                                  Z2 is AnkunftFahrer,
                                                  greater_than(Z1,Z2,Ankunft),
                                                  Abfahrt is Ankunft-Cost.

% aktuelle zeit aus der route ablesen
actualTime(Route,ActualTime) :- last(Route,[_,_,ActualTime,_]).

%liest startzeit der Person aus
getstart(Person,Time) :- times(Person,Time,_).

greater_than(A,B,X) :- A>=B,X=A.
greater_than(A,B,X) :- A<B,X=B.


persons_todo(Route,Todo) :- person_done(Route,Y),
                           person_set(X),
                           subtract(X,Y,Todo).
person_set(X):-setof(Y,p(Y),X).
person_done(Route,X) :- setof(Personen,get_person(Route,Personen),X).
get_person(Route,Personen) :-  member(X,Route),
                                last(X,Personen).
get_person(Route,Personen) :-  member([Personen|_],Route).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Snippets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%print_first(X) :- all_list([X|S]).
%print_second(X) :- print_first([X|S]).

%all_return([X,Y,Z]) :- l(X),t(Y),l(Z),dif(X,Z).
%all_list(Z) :- setof(X,all_return(X),Z).