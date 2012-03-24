% Author:
% Date: 23.03.2012

person(steve).
person(bill).
%person(wallace).
%person(grommit).
%person(asterix).
%person(obelix).

hat_fuehrerschein(steve).

mache_master_plan().

/*
domains

  town = symbol
  distance = unsigned
  rib = r(town,town,distance)
  tlist = town*
  rlist = rib*

predicates

  nondeterm way(town,town,rlist,distance)
  nondeterm route(town,town,rlist,tlist,distance)
  nondeterm route1(town,tlist,rlist,tlist,distance)
  nondeterm ribsmember(rib,rlist)
  nondeterm townsmember(town,tlist)
  nondeterm tsp(town,town,tlist,rlist,tlist,distance)
  nondeterm ham(town,town,tlist,rlist,tlist,distance)
  nondeterm shorterRouteExists(town,town,tlist,rlist,distance)
  nondeterm alltown(tlist,tlist)
  nondeterm write_list(tlist)
*/