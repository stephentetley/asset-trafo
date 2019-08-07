% translate_floc.pl

:- module(translate_floc,
    [
      atoms_to_floc/2
    , floc_take/3

    , process_group_installation_root/2
    , process_installation_root/2

    , aib_installation_to_s4_floc/2
    , aib_process_group_to_s4_floc/2
    , aib_process_to_s4_floc/2

    , aib_floc_below/2
    , aib_equipment_below/2

    , aib_floc_to_s4_system/2
    , aib_equipment_to_s4_system/2

    , aib_to_s4_floc/2

    ]).

:- use_module(floc_rules/floc_rule_mapping_1_2).
:- use_module(floc_rules/floc_rule_mapping_2_3).
:- use_module(floc_rules/floc_rule_mapping_2_3_4).

%% Ideally this would be somehow dynamic 
:- use_module(sample_data/aib_inst_facts1).
:- use_module(sample_data/s4_site_facts1).
:- use_module(sample_data/s4_equipment_facts1).

atoms_to_floc(List, Floc) :- 
    atomics_to_string(List, "-", Floc).

% if count > len take whole list.
list_take(Src, Count, List) :- 
    findall(Elt, (nth1(Ix, Src, Elt), Ix =< Count), List).

floc_take(Src, Len, Floc) :- 
    atomics_to_string(List, "-", Src),
    list_take(List, Len, List1),
    atomics_to_string(List1, "-", Floc).


norm_floc_part('', '@@@') :- !.
norm_floc_part(Part, Part) :- !.



process_group_installation_root(PgKey, InstKey) :- 
    aib_process_group(PgKey, _, _, InstKey).

process_installation_root(PKey, InstKey) :- 
    aib_process(PKey, _, _, PgKey),
    aib_process_group(PgKey, _, _, InstKey),
    !.

% no intermediate process group
process_installation_root(PKey, InstKey) :- 
    aib_process(PKey, _, _, InstKey),
    aib_installation(InstKey, _, _).


aib_installation_to_s4_floc(Sai, Floc) :- 
    aib_inst_floc1_floc2(Sai, X, Y), 
    atoms_to_floc([X,Y], Floc).

aib_process_group_to_s4_floc(Sai, Floc) :- 
    process_group_installation_root(Sai, Inst),
    aib_installation(Inst, _, InstType),
    aib_process_group(Sai, PgName, _, _),
    aib_inst_floc1_floc2(Inst, X, Y), 
    aib_stype_procg_s4_fun_procg(InstType, PgName, Z1, Z2),
    norm_floc_part(Z1, ZZ1),
    norm_floc_part(Z2, ZZ2),
    atoms_to_floc([X, Y, ZZ1, ZZ2], Floc).


process_to_process_group_name(Sai, PgName) :-
    aib_process(Sai, _, _, PgKey),
    aib_process_group(PgKey, PgName, _, _),
    !.

process_to_process_group_name(Sai, PgName) :-
    aib_process(Sai, _, _, InstKey),
    aib_installation(InstKey, _, _),
    PgName = '',
    !.


aib_process_to_s4_floc(Sai, Floc) :- 
    process_installation_root(Sai, Inst),
    aib_installation(Inst, _, InstType),
    aib_process(Sai, PName, _, _),
    process_to_process_group_name(Sai, PgName),
    aib_inst_floc1_floc2(Inst, X, Y), 
    aib_stype_procg_proc_s4_fun_procg_proc(InstType, PgName, PName, Z1, Z2, Z3),
    writeln([Z1,Z2,Z3]),
    norm_floc_part(Z1, ZZ1),
    norm_floc_part(Z2, ZZ2),
    norm_floc_part(Z3, ZZ3),
    atoms_to_floc([X, Y, ZZ1, ZZ2, ZZ3], Floc).

%%% 

aib_floc_below(Parent, Child) :-
    aib_plant_item(Child, _, _, Parent).

% one below plant
aib_floc_below(Parent, Child) :-
    aib_plant(Child, _, _, Parent).    

% below-below plant
aib_floc_below(Parent, Child) :-
    aib_plant(X1, _, _, Parent), 
    aib_floc_below(X1, Child).

% one below process
aib_floc_below(Parent, Child) :-
    aib_process(Child, _, _, Parent).  

% below-below process
aib_floc_below(Parent, Child) :-
    aib_process(X1, _, _, Parent),
    aib_floc_below(X1, Child).

% one below process_group
aib_floc_below(Parent, Child) :-
    aib_process_group(Child, _, _, Parent).  

% below-below process_group
aib_floc_below(Parent, Child) :-
    aib_process_group(X1, _, _, Parent),
    aib_floc_below(X1, Child).   

% This case should capture aib_plant_item and any other type
% with directly attached equipment.
aib_equipment_below(SaiCode, PliCode) :- 
    aib_equipment(PliCode, _, _, SaiCode).

aib_equipment_below(SaiCode, PliCode) :- 
    aib_floc_below(SaiCode, Kid1),
    aib_equipment_below(Kid1, PliCode).

%%% 

aib_floc_to_s4_system_aux(SaiCode, SysFloc) :-
    s4_assembly(_, _, _, SaiCode, _, _, SysFloc),
    !.

aib_floc_to_s4_system_aux(SaiCode, SysFloc) :-
    s4_item(_, _, _, SaiCode, _, _, _, UFloc),
    s4_assembly(UFloc, _, _, _, _, _, SysFloc),
    !.

% May return more than one result at the higher levels in the tree.
% Should be deterministic for plant and plant item.
aib_floc_to_s4_system(SaiCode, SysFloc) :- 
    aib_floc_to_s4_system_aux(SaiCode, SysFloc).

aib_floc_to_s4_system(SaiCode, SysFloc) :- 
    aib_floc_below(SaiCode, Kid1),
    aib_floc_to_s4_system(Kid1, SysFloc).

%%%

aib_equipment_to_s4_system(PliCode, SysFloc) :-
    s4_assembly(_, _, _, _, _, PliCode, SysFloc).

aib_equipment_to_s4_system(PliCode, SysFloc) :-
    s4_item(_, _, _, _, _, _, PliCode, UFloc),
    s4_assembly(UFloc, _, _, _, _, _, SysFloc).


aib_equipment_to_s4_assembly(PliCode, AsmFloc) :-
    s4_assembly(AsmFloc, _, _, _, _, PliCode, _).

aib_equipment_to_s4_assembly(PliCode, AsmFloc) :-
    s4_item(_, _, _, _, _, _, PliCode, AsmFloc),
    s4_assembly(AsmFloc, _, _, _, _, _, _).


aib_equipment_to_s4_item(PliCode, ItemFloc) :-
    s4_item(ItemFloc, _, _, _, _,  _, PliCode, _).


aib_equipment_to_s4_floc(PliCode, Floc) :- 
    s4_equipment(_, PliCode, _, _, _, _, Floc).


%%%

% installation retrieves a length 1 floc (site)
aib_to_s4_floc_aux(Ref, Floc) :- 
    aib_installation(Ref, _, _),
    aib_equipment_below(Ref, Equip), 
    aib_equipment_to_s4_floc(Equip, Floc1),
    floc_take(Floc1, 1, Floc).

% process_group retrieves a length 3 floc (process group)
aib_to_s4_floc_aux(Ref, Floc) :- 
    aib_process_group(Ref, _, _, _),
    aib_equipment_below(Ref, Equip), 
    aib_equipment_to_s4_floc(Equip, Floc1),
    floc_take(Floc1, 3, Floc).


% process retrieves a length 4 floc (process)
aib_to_s4_floc_aux(Ref, Floc) :- 
    aib_process(Ref, _, _, _),
    aib_equipment_below(Ref, Equip), 
    aib_equipment_to_s4_floc(Equip, Floc1),
    floc_take(Floc1, 4, Floc).

% plant retrieves a length 6 floc (assembly)
aib_to_s4_floc_aux(Ref, Floc) :- 
    aib_plant(Ref, _, _, _),
    aib_equipment_below(Ref, Equip), 
    aib_equipment_to_s4_floc(Equip, Floc1),
    floc_take(Floc1, 6, Floc).


% plant_item retrieves a length 7 floc (item)
aib_to_s4_floc_aux(Ref, Floc) :- 
    aib_plant_item(Ref, _, _, _),
    aib_equipment_below(Ref, Equip), 
    aib_equipment_to_s4_floc(Equip, Floc1),
    floc_take(Floc1, 7, Floc).

% Equipment floc is variable length (no truncation)
aib_to_s4_floc_aux(Ref, Floc) :-
    s4_equipment(_, Ref, _, _, _, _, Floc).


aib_to_s4_floc(Ref, Flocs) :- 
    setof(X, aib_to_s4_floc_aux(Ref,X), Flocs).


