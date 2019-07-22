% equipment_below_floc.lp

% To run from the clingo toplevel ...
% > clingo facts/base_asset_type.lp facts/rule_table_equipment.lp facts/rule_table_funcloc.lp queries/equipment_below_floc.lp


% type_of(X) :- base_asset_type("BUILDINGS + STRUCTURES", X).

% #show type_of/1.


descendent_floc(X,Y) :- funcloc(X,Y).
descendent_floc(X,Z) :- descendent_floc(X,Y), descendent_floc(Y,Z).

descendent_floc_with_type(X,Y,Z) :- descendent_floc(X,Y), base_asset_type(Y,Z).


equip_below(X, Ans) :-
    descendent_floc(X, Kid),
    eqpt(Kid, Ans).

get_equip_below(Ans) :- equip_below("6XDWF OVERFLOW", Ans).

get_descendents(Ans, Typ) :- descendent_floc_with_type("6XDWF OVERFLOW", Ans, Typ).

get_type(X) :- base_asset_type("6XDWF OVERFLOW", X).

#show get_equip_below/1.

#show get_descendents/2.

#show get_type/1.