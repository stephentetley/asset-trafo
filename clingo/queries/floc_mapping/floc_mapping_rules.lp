% floc_mapping_rules.lp



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
    level234_mapping(InstType, Procg, Proc, F2, F3, F4).

