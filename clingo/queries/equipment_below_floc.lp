% equipment_below_floc.lp

% To run from the clingo toplevel ...

% bash> clingo -c qvar_asset='"VALVE"' facts/aib_base_asset_type.lp facts/aib_rule_table_equipment.lp facts/aib_rule_table_funcloc.lp queries/equipment_below_floc.lp 


#const qvar_asset = "VALVE".

% Rules

descendant_floc(X,Y) :- funcloc(X,Y).
descendant_floc(X,Z) :- descendant_floc(X,Y), descendant_floc(Y,Z).

descendant_floc_with_type(X,Y,Z) :- descendant_floc(X,Y), base_asset_type(Y,Z).

% Need two cases for equipment below.
% 1. Equipment below kids of item
% 2. equipment below item itself

equip_below(X, Ans) :-
    descendant_floc(X, Kid),
    eqpt(Kid, Ans).

equip_below(X, Ans) :-
    eqpt(X, Ans).    

% Queries

identity(qvar_asset).

get_equip_below(Ans) :- equip_below(qvar_asset, Ans).

get_floc_below(Cat, Ans) :- descendant_floc_with_type(qvar_asset, Ans, Cat).

get_type(X) :- base_asset_type(qvar_asset, X).

#show identity/1.

#show get_equip_below/1.

#show get_floc_below/2.

#show get_type/1.