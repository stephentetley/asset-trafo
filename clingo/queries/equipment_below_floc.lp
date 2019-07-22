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

get_equip_below(Ans) :- equip_below("VALVE", Ans).

get_descendents(Cat, Ans) :- descendent_floc_with_type("VALVE", Ans, Cat).

get_type(X) :- base_asset_type("VALVE", X).

#show get_equip_below/1.

#show get_descendents/2.

#show get_type/1.