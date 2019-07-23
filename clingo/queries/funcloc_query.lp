% funcloc_query.lp

% To run from the clingo toplevel ...
% > clingo facts/rule_table_funcloc.lp facts/base_asset_type.lp queries/funcloc_query.lp

descendent_floc(X,Y) :- funcloc(X,Y).
descendent_floc(X,Z) :- descendent_floc(X,Y), descendent_floc(Y,Z).

descendent_floc_with_type(X,Y,Z) :- descendent_floc(X,Y), base_asset_type(Y,Z).



test(Ans) :- descendent_floc("WASTE WATER CROSSING", Ans).

#show test/1.

%*

test(Ans,Typ) :- descendent_floc_with_type("WASTE WATER CROSSING", Ans, Typ).

test_plant(Ans, "PLANT") :- 
    descendent_floc_with_type("WASTE WATER CROSSING", Ans, Typ),
    Typ = "PLANT".

% #show descendent/2.

#show test_plant/2.

test_typeof(Typ) :- base_asset_type("SLAB", Typ).

#show test_typeof/1.

*%