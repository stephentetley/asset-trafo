% floc_mapping_query.lp

% To run from the clingo toplevel ...
% > clingo facts/floc_mapping_1_2.lp facts/aib_common_names.lp queries/floc_mapping_query.lp


% This is bi-directional.
map_installation_to_site(AibName, S4Name) :-
    aib_common_name(X, AibName),
    level1_mapping(X, _, S4Name).

test_get_s4_site(Ans) :- map_installation_to_site("ARKWRIGHT/CSO", Ans).

test_get_aib_installation(Ans) :- map_installation_to_site(Ans, "Arkwright SPS").

#show test_get_s4_site/1.
#show test_get_aib_installation/1.



