% Author: Martin Eckleben, Andre Lung
% Date: 23.03.2012

/*
p -> person
l -> location
b -> bar
f -> hat führerschein
se -> start / endpunkt
*/

/**********************************
Config
***********************************/

%Personen
p(X) :- times(X,_).
times(steve,1200).             %(name,ab wann kann er)
times(bill,1400).
times(hans,1300).
times(guido,1500).
times(ingo,800).
f(steve).                      %hat führerschein
f(guido).
f(ingo).

%Bars
b(mata).
b(oblomow).
b(hacienda).

%Sperrstunde, bis wann haben die Bars geöffnet?
sperrstunde(2400).

%Gruppierungen
se(X) :- p(X),f(X).            %start und endpunkt kann nur eine person sein die einen Führerschein hat
l(X) :- p(X).                  %eine person ist eine location
l(X) :- b(X).                  %eine bar ist eine location

%Wegenetz
cost(hans,bill,50).
cost(hans,steve,50).
cost(hans,mata,25).
cost(hans,oblomow,75).
cost(hans,guido,25).
cost(hans,ingo,500).
cost(hans,hacienda,83).

cost(bill,steve,75).
cost(bill,mata,25).
cost(bill,oblomow,50).
cost(bill,guido,25).
cost(bill,ingo,75).
cost(bill,hacienda,12).

cost(mata,steve,25).
cost(mata,oblomow,50).
cost(mata,guido,50).
cost(mata,ingo,10).
cost(mata,hacienda,69).

cost(steve,oblomow,50).
cost(steve,guido,25).
cost(steve,ingo,500).
cost(steve,hacienda,166).

cost(oblomow,guido,25).
cost(oblomow,ingo,12).
cost(oblomow,hacienda,89).

cost(guido,ingo,50).
cost(guido,hacienda,45).

cost(ingo,hacienda,5).

get_cost(X,Y,Z) :- cost(X,Y,Z).
get_cost(X,Y,Z) :- cost(Y,X,Z).



/*****************************
Main
*****************************/

go(FINAL_ROUTE) :-
  start_set(Start),                                       %Liste mit Routenstarts generieren
  iterate_routelist(Start, AllRoutes),                    %Alle Abholrouten berechnen
  get_best(AllRoutes,BestRoute),                          %Beste Route auswaehlen
  add_bars(BestRoute, BarList),                           %Barroute inkl. Aufenthalt generieren
  deliver_persons(BestRoute,BarList,HomeRoute),           %Personen heimfahren
  append(BestRoute,BarList,X),
  append(X,HomeRoute,FINAL_ROUTE).
  








/****************************
Personen abholen
****************************/

get_latest_start([],0).
get_latest_start([[[_,New_val,_,_]|_]|Rest],L):-
    get_latest_start(Rest,Old_val),
    greater_than(New_val,Old_val,L).

get_routes_with_latest_start(In,Number,Out):-
 member(X,In),                               %hole dir jede route
 getfirst(X,[_,One,_,_]),                    %und davon das erste element
 Number==One,
 Out=X.

get_earliest_arrival_number([],99999999).
get_earliest_arrival_number([First|Rest],L):-
    get_earliest_arrival_number(Rest,Old_val),
    last(First,[_,_,New_val,_]),
    smaller_than(New_val,Old_val,L).

get_routes_with_earliest_arrival(In,Number,Out):-
 member(X,In),
 last(X,[_,_,One,_]),
 Number==One,
 Out=X.

%appends an element to a list if the first values are equal
addit(One,Two,Element,OldList,NewList) :- One=Two,hinten_anfuegen(Element,OldList,NewList).

%add element to list
hinten_anfuegen(Element, [], [Element]).
hinten_anfuegen(Element, [Kopf|Rest], [Kopf|Neueliste]):-
                    hinten_anfuegen(Element, Rest, Neueliste).

%[[[a,10,14,b],[b,14,19,c],[c,19,25,d]],[[e,10,14,f],[f,14,19,g],[g,21,28,h]],[[x,12,14,y],[y,14,19,z],[z,20,25,q]],[[t,12,14,u],[u,14,19,v],[v,20,40,w]]]
get_best(Routen,R):-
 get_earliest_arrival_number(Routen,Number),
 setof(X,get_routes_with_earliest_arrival(Routen,Number,X),Y),
 get_latest_start(Y,Latest),
 setof(Z,get_routes_with_latest_start(Y,Latest,Z),Second),
 getfirst(Second,R).

start_set(Out) :-
  setof(X,starts(X),Out).

starts([[Startpunkt,Abfahrt,Ankunft,Ziellocation]]) :-
  se(Startpunkt),
  p(Ziellocation),                                              %erstes ziel muss Person sein! (alleine trinken macht keinen Spaß :) )
  dif(Startpunkt,Ziellocation),                                 %start und ziel müssen verschieden sein
  earliest_time(Startpunkt,Ziellocation,Abfahrt,Ankunft).       %frühestmöglicher zeitpunkt

%iterate_routelist determines, if there are any persons to catch and continues or ends the recursion                                                                                    
iterate_routelist(Routelisttodo, Routelistdone) :-
 [Testroute | _] = Routelisttodo,                                                               %extract one testroute
 persons_todo(Testroute, Persons_todo),
 length(Persons_todo,X),
 do_next(X, Routelisttodo, Routelistdone).                              %Case
 
%do_next is something like a case in prolog :) 
do_next(0, Routelisttodo, Routelistdone) :-                                             %End recursion, if zero 
 Routelistdone = Routelisttodo.
do_next(_, Routelisttodo, Routelistdone) :-                                             %go on, if there are waypoints to do
 routelist_iterator(Routelisttodo, [], Temp),
 iterate_routelist(Temp, Routelistdone).

routelist_iterator([]        , RoutesDone, NewRoutelist) :- NewRoutelist = RoutesDone.
routelist_iterator(RoutesTodo, RoutesDone, NewRoutelist) :-
 [OneRoute | Todo] = RoutesTodo,                                                                % [Eine Route] | [AlleUebrigenRouten]
 next_moves([OneRoute], [], OneRouteDone),                                              % 
 append(RoutesDone, OneRouteDone, NewRoutesDone),                               
 routelist_iterator(Todo, NewRoutesDone, NewRoutelist).
 %NewRoutelist = NewRoutesDone.

%next_moves erhaelt eine Routenliste und iteriert jede Route der Liste um genau einen Wegpunkt
next_moves([], Moves_done, New_routelist) :- 
 New_routelist = Moves_done.
next_moves(Last_routelist, Moves_done, New_routelist) :-
  [Single_route | Todo] = Last_routelist,
  one_next_move(Single_route, New_routes),
  append(Moves_done, New_routes, Done),
  next_moves(Todo, Done, New_routelist).                                 %Rekursiver Aufruf

%one_next_move delivers the arguments to add_person.
one_next_move(Single_route, All_next_moves)  :-                       %hole personen ab
  persons_todo(Single_route, Persons_todo),
  add_person(Single_route,  Persons_todo, [], All_next_moves).

%add_person adds all possible next waypoint to a given route
add_person(_,[], Temp, Possible_next_moves) :-
  Possible_next_moves = Temp.                                             %Wenn keine Person mehr abzuholen ist -> fertig.

add_person(One_route, Persons_todo, Temp, Possible_next_moves) :-
  [NextPerson | Todo ] = Persons_todo,
  last_location(One_route, Last_location),
  actualTime(One_route, ActualTime),
  relative_earliest_time(Last_location,NextPerson,ActualTime,AbfahrtAlt,AnkunftNeu),
  append(One_route,[[Last_location,AbfahrtAlt,AnkunftNeu,NextPerson]],New_route),
  append([New_route], Temp, Done),
  add_person(One_route, Todo, Done, Possible_next_moves).









/**********************************
BARS
Partylustige Menschen suchen immer die nächstbeste Bar und wollen bis zur Sperrstunde überall mal gewesen sein
***********************************/

add_bars(BestList, BarList) :-                                  %BestList hat die Form [[x,5,y],...[z,8,t]]
 start_bar(BestList, Startpoint),                               %generiert den ersten Punkt in Form [[Person, 1600, 1700, Bar]]
 iterate_barlist(Startpoint, BarListTimeless),
 maximize_bartime(BestList, BarListTimeless, BarList). 
 
%iterate_barlist determines, if there are any persons to catch and continues or ends the recursion                                                                                      
iterate_barlist(Route_todo, Routelistdone) :-
 bars_todo(Route_todo, Bars_todo),
 length(Bars_todo,X),
 do_nextbar(X, Route_todo, Routelistdone).                              %Case
 
%do_nextbar is something like a case in prolog :) 
do_nextbar(0, Routelisttodo, Routelistdone) :-                                          %End recursion, if zero 
 Routelistdone = Routelisttodo.
do_nextbar(_, Routelisttodo, Routelistdone) :-                                          %go on, if there are waypoints to do
 barlist_iterator(Routelisttodo, Temp),
 iterate_barlist(Temp, Routelistdone). 

barlist_iterator(Lastroute, Newroute) :-
 last_location(Lastroute, Lastlocation),
 bars_todo(Lastroute, Bars_todo),
 get_nearest_bar(Lastlocation, Bars_todo, Nearest_bar),
 add_bar(Lastroute, Nearest_bar, Newroute).
 
add_bar(Lastroute, Nearest_bar, Newroute) :-
 last_location(Lastroute, Lastlocation),
 get_cost(Lastlocation, Nearest_bar, Cost),
 actualTime(Lastroute, Abfahrt),
 Ankunft is Abfahrt + Cost,
 append(Lastroute,[[Lastlocation,Abfahrt,Ankunft,Nearest_bar]],Newroute).

start_bar(BestList, Startpoint) :-
 last_location(BestList, Lastlocation),
 findall(X, b(X), Bars),
 get_nearest_bar(Lastlocation, Bars, Bestbar),
 get_cost(Lastlocation, Bestbar, Cost),
 Startpoint = [[Lastlocation, 0, Cost, Bestbar]].
 
%maximize_bartime berechnet die maximale Aufenthaltsdauer pro Bar und ruft add_bartime auf
maximize_bartime(BestList, BarListTimeless, BarList) :-
 sperrstunde(Sperrstunde),
 actualTime(BestList,Actualtime),
 Resttime is Sperrstunde - Actualtime,
 get_route_duration(BarListTimeless, Drivetime),
 Usabletime is Resttime - Drivetime,
 length(BarListTimeless, Waypointcount),                                %number of bars todo
 Timeprobar is Usabletime / Waypointcount,                              %Aufenthaltszeit pro Bar
 add_bartime(BarListTimeless, Sperrstunde, Timeprobar, [], BarList).
 
%add_bartime fuegt bei jeder Bar einen Aufenthalt in Hoehe von "Timetoadd" hinzu
add_bartime([]         , _   , _        , Temp, BarList) :-
 BarList = Temp.
add_bartime(BarListTodo, Time, Timetoadd, Temp, BarList) :-
 last(BarListTodo, Waypoint),                                                           %letztes Element auswählen
 select(Waypoint, BarListTodo, NewTodo),                                                %letztes Element entfernen
 [AlteLoc, AbAlt, AnNeu, NeueLoc] = Waypoint,                                           %alte Zeiten auslesen
 GoodAbAlt is Time - Timetoadd + AbAlt - AnNeu,                                         %korrekte Abfahrt bei AlteLoc berechnen
 GoodAnNeu is Time - Timetoadd,                                                         %korrekte Ankunft an NeueLoc berechnen
 NewWaypoint = [[AlteLoc, GoodAbAlt, GoodAnNeu, NeueLoc]],                              %Aktualisierten Wegpunkt generieren
 append(NewWaypoint, Temp, Next),                                                       %Wegpunkt hinten anhaengen
 add_bartime(NewTodo, GoodAbAlt, Timetoadd, Next, BarList).                             %rekursiver Aufruf

%returns the nearest bar
get_nearest_bar(Lastlocation, Bars_todo, Bestbar) :-
 next_bars(Lastlocation, Bars_todo, [], Barlist),
 get_smallest_number(Barlist, Smallestnumber),
 get_bar_with_number(Smallestnumber, Barlist, Best),
 [Bestbar | _ ] = Best.
 
get_smallest_number([],99999999).
get_smallest_number([First|Rest],L):-
    get_smallest_number(Rest,Old_val),
    [New_val | _ ] = First,
    smaller_than(New_val,Old_val,L).

get_bar_with_number(Number, Barlist, Bar) :-
 member([Number|Bar], Barlist).

%next_bars generates a list like [[50, bar1], [80, bar2]]. The number defines the cost
next_bars(_                       , []           , Done, Barlist) :- 
 Barlist = Done.
next_bars(Lastlocation, Bars_todo, Done, Barlist) :-
 [Onebar | Rest] = Bars_todo,
 get_cost(Lastlocation, Onebar, Cost),
 append(Done, [[Cost, Onebar]], Appended),
 next_bars(Lastlocation, Rest, Appended, Barlist).
 
bars_todo(Testroute, Bars_todo) :- 
 bars_done(Testroute, Bars_done),
 setof(X, b(X), Barset),
 subtract(Barset, Bars_done, Bars_todo).
 
bars_done(Testroute, Bars_done) :-
 setof(Bars,get_bars(Testroute,Bars),Bars_done).
 
get_bars(Route,Bars) :-  
 member(X, Route),
 last(X, Bars).
get_bars(Route, Bars) :-
 member([Bars|_],Route). 
 
 
 
 
 
 

/****************************
Heimfahrt
****************************/

%bestlist beispiel [[steve,1200,1300,hans],[hans,1300,1400,bill],[bill,1450,1500,guido]]
%Reversed beispiel [[guido,1500,1450,bill], [bill,1400,1300,hans], [hans,1300,1200,steve]]

%barlist beispiel [[guido,1500,1550,oblomow],[oblomow,1600,1700,mata]]
%heimroute beispiel [[mata,1900,2000,guido],[guido,2000,2050,bill],[bill,2050,2150,hans],[hans,2150,2250,steve]]

deliver_persons(BestList,BarList,HomeRoute):-
deepreverse(BestList,Reversed),
calculate_bindeglied(Reversed,BarList,Bindeglied),
[_,_,Start,_]=Bindeglied,
correct_times(Start,Reversed,[],X),
append([Bindeglied],X,HomeRoute).

calculate_bindeglied(Reversed,BarList,Bindeglied):-
 last(BarList,[_,_,_,Bar]),                  %hole dir letzte bar
 getfirst(Reversed,[Person,_,_,_]),          %hole dir erste anzufahrende person
 get_cost(Bar,Person,Kosten),                %hole kosten von letzter bar zu erster person
 sperrstunde(Abfahrt),                       %Bestimme Zeitpunkt der Abfahrt
 Bindeglied = [Bar,Abfahrt,Kosten,Person].   %errechne das Bindeglied (erste Person)

%Aufruf: correct_times(IN, [], OUT).
correct_times(_,[],Done, Done).
correct_times(Start,Todolist, Done, Result) :-
 [OneElement | Rest] = Todolist,
 [From,_,_,To] = OneElement,
 get_cost(From,To,Kosten),
 Ankunft is Start+Kosten,
 ElementDone=[[From,Start,Ankunft,To]],
 append(Done, ElementDone, Appended),
 correct_times(Ankunft,Rest, Appended, Result).







/**********************************
Hilfsfunktionen
***********************************/
get_route_duration(A,B) :- 
  last(A,[_,_,End,_]), 
  getfirst(A,[_,Start,_,_]), 
  B is End-Start.

%spiegelt eine Liste und ihre Teillisten
deep_reverse_list(A,B):-
 reverse(A,X),
 deep_reverse(X,B).
 
deepreverse(L,LRev) :- deepreverse_h(L,LRev,[]).
deepreverse_h([], LRev, LRev) :- !.
deepreverse_h([E|R],LRev,LTemp) :-
 ((atomic(E),EN=E) ; deepreverse(E,EN) ),
 deepreverse_h(R,LRev,[EN|LTemp]).

%last_location liest den letzten Ort aus der Route aus.
last_location(Route,X) :-
 last(Route,Y),
 last(Y,X).

%relative_earliest_time bestimmt den bestmoeglichen Abfahrtszeitpunkt zwischen gegebenem Ort und neuem Ziel.
relative_earliest_time(AlterOrt, NeuerOrt,ActualTime,AbfahrtAltesZiel,AnkunftGast) :-
 getstart(NeuerOrt, EarliestGuestStart),                                                         %frühester start bei Gast
 get_cost(AlterOrt,NeuerOrt,Cost),                                                               %Kosten für Strecke
 EarliestDriverArrival is ActualTime+Cost,                                                       %
 greater_than(EarliestDriverArrival,EarliestGuestStart,AnkunftGast),
 AbfahrtAltesZiel is AnkunftGast-Cost.

%earliest_time bestimmt im ersten Schritt den besten Abfahrt und Ankunftszeitpunkt zwischen zwei Personen
earliest_time(Person1,Person2,Abfahrt,Ankunft) :-
 get_cost(Person1,Person2,Cost),
 getstart(Person1,AbfahrtFahrer),
 getstart(Person2,AnkunftFahrer),
 Z1 is AbfahrtFahrer + Cost,
 Z2 is AnkunftFahrer,
 greater_than(Z1,Z2,Ankunft),
 Abfahrt is Ankunft-Cost.

%actualTime liest die aktuelle zeit aus der route aus
actualTime(Route,ActualTime) :- last(Route,[_,_,ActualTime,_]).

%getstart liest die Startzeit der Person aus
getstart(Person,Time) :- times(Person,Time).

%persones_todo liest aus einer Route aus, welche Personen noch nicht abgeholt wurden
persons_todo(Route,Todo) :- person_done(Route,Y),
                           person_set(X),
                           subtract(X,Y,Todo).
person_set(X):-setof(Y,p(Y),X).
person_done(Route,X) :- setof(Personen,get_person(Route,Personen),X).
get_person(Route,Personen) :-  member(X,Route),
                               last(X,Personen).
get_person(Route,Personen) :-  member([Personen|_],Route).

%greater_than
greater_than(A,B,X) :- A>=B,X=A.
greater_than(A,B,X) :- A<B,X=B.

%smaller_than
smaller_than(A,B,X) :- A<B,X=A.
smaller_than(A,B,X) :- A>=B,X=B.

get_time_of_route(A,B):-
 last(A,[_,_,End,_]),
 getfirst(A,[_,Start,_,_]),
 B is End-Start.

getfirst([First|_],First).