% common.pl

is_aib_installation(Sai) :- 
    aib_floc_l1_l2_installation(Sai, _, _).

is_aib_process_group(Sai) :- 
    aib_floc_l3_process_group(Sai, _, _, _, _).

is_aib_process(Sai) :- 
    aib_floc_l4_process(Sai, _, _, _, _).

is_aib_plant(Sai) :- 
    aib_floc_l5_plant(Sai, _, _, _, _).

aib_installation_sai_name(Sai, Name) :- 
    aib_floc_l1_l2_installation(Sai, Name, _).

aib_installation_sai_type(Sai, Type) :- 
    aib_floc_l1_l2_installation(Sai, _, Type).