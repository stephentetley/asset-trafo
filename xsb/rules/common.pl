% common.pl

aib_installation_sai_name(Sai, Name) :- 
    aib_floc_l1_l2_installation(Sai, Name, _).

aib_installation_sai_type(Sai, Type) :- 
    aib_floc_l1_l2_installation(Sai, _, Type).