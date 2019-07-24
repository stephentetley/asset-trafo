% query1.lp


% To run from the clingo toplevel ...
% > clingo facts/floc_mapping_2_3_4.lp facts/aib_common_names.lp facts/aib_installation_type.lp facts/floc_mapping_1_2.lp queries/floc_mapping/floc_mapping_rules.lp queries/floc_mapping/query1.lp

#const argv1 = "ARKENDALE/WPS".
#const argv2 = "ODOUR CONTROL".
#const argv3 = "ODOUR FILTERING".


floc4_fwd(F1, F2, F3, F4) :- 
    floc4(F1, F2, F3, F4, argv1, argv2, argv3).
#show floc4_fwd/4.



%*
test_floc1(Ans) :- floc1("ANC02", Ans).
#show test_floc1/1.


test_floc2(Ans) :- floc2("ARK01", "WTN", Ans).
#show test_floc2/1.



test_floc4_rev(S, PG, P) :- floc4("ARK01", "WTN", "WTF","OAC", S, PG, P).
#show test_floc4_rev/3.

test_floc4_fwd(F1, F2, F3, F4) :- 
    floc4(F1, F2, F3, F4, "ARKENDALE/WPS","ODOUR CONTROL","ODOUR FILTERING").
#show test_floc4_fwd/4.


test_floc3_fwd(F1, F2, F3) :- 
    floc3(F1, F2, F3, "ARKENDALE/WPS","ODOUR CONTROL").
#show test_floc3_fwd/3.

*%