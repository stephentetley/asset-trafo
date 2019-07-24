% temp_mapping.lp


% To run from the clingo toplevel ...
% > clingo facts/floc_mapping_2_3_4.lp facts/aib_common_names.lp facts/aib_installation_type.lp facts/floc_mapping_1_2.lp queries/floc_mapping2.lp



floc1(F1, Site) :- 
    level1_mapping(Sai, F1, _),
    aib_common_name(Sai, Site).


floc2(F1, F2, Site) :- 
    level2_mapping(Sai, F1, F2),
    aib_common_name(Sai, Site).  


% Is floc3 just a non-det lfloc4?
floc3(F1, F2, F3, Site, Procg) :- 
    floc4(F1, F2, F3, _, Site, Procg, _).


floc4(F1, F2, F3, F4, Site, Procg, Proc) :- 
    floc1(F1, Site),
    level1_mapping(Sai, F1, _),
    aib_installation_type(Sai, InstType),
    code_mapping(InstType, Procg, Proc, F2, F3, F4).

%*
test_floc1(Ans) :- floc1("ANC02", Ans).
#show test_floc1/1.


test_floc2(Ans) :- floc2("ARK01", "WTN", Ans).
#show test_floc2/1.
*%


test_floc4_rev(S, PG, P) :- floc4("ARK01", "WTN", "WTF","OAC", S, PG, P).
#show test_floc4_rev/3.

test_floc4_fwd(F1, F2, F3, F4) :- 
    floc4(F1, F2, F3, F4, "ARKENDALE/WPS","ODOUR CONTROL","ODOUR FILTERING").
#show test_floc4_fwd/4.


test_floc3_fwd(F1, F2, F3) :- 
    floc3(F1, F2, F3, "ARKENDALE/WPS","ODOUR CONTROL").
#show test_floc3_fwd/3.