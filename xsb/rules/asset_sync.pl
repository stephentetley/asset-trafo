% asset_sync.pl

% see __load_clipboard, must load ['rules/common.pl'].

% use abstract floc codes... ['rules/floc_codes.pl'].

% ['rules/asset_sync.pl'].

% get aib_equipment below a floc code


:- table aib_floc_below/2.


aib_floc_below(Parent, Child) :-
    aib_floc_l6_plant_item(Child, _, _, _, Parent).

% one below plant
aib_floc_below(Parent, Child) :-
    aib_floc_l5_plant(Child, _, _, _, Parent).    

% below-below plant
aib_floc_below(Parent, Child) :-
    aib_floc_below(X1, Child),
    aib_floc_l5_plant(X1, _, _, _, Parent).    



% one below process
aib_floc_below(Parent, Child) :-
    aib_floc_l4_process(Child, _, _, _, Parent).  

% below-below process
aib_floc_below(Parent, Child) :-
    aib_floc_below(X1, Child),
    aib_floc_l4_process(X1, _, _, _, Parent). 


% one below process_group
aib_floc_below(Parent, Child) :-
    aib_floc_l3_process_group(Child, _, _, _, Parent).  

% below-below process_group
aib_floc_below(Parent, Child) :-
    aib_floc_below(X1, Child),
    aib_floc_l3_process_group(X1, _, _, _, Parent).     

% should pass
% aib_floc_below('SAI00003608', X).


% Should fail.
% aib_floc_below('SAI00215603', X).

% Should pass
% aib_floc_below('SAI00396905', X).


:- table aib_equipment_below/2.

% This case should capture aib_plant_item and any other type
% with directly attached equipment.
aib_equipment_below(SaiCode, PliCode) :- 
    aib_equipment(PliCode, _, _, _, SaiCode).

aib_equipment_below(SaiCode, PliCode) :- 
    aib_floc_below(SaiCode, Kid1),
    aib_equipment_below(Kid1, PliCode).

%% 




aib_floc_to_s4_system(SaiCode, SysFloc) :-
    s4_floc_l6_assembly(_, _, _, SaiCode, _, SysFloc).

aib_floc_to_s4_system(SaiCode, SysFloc) :-
    s4_floc_l7_item(_, _, _, SaiCode, _, _, UFloc),
    s4_floc_l6_assembly(UFloc, _, _, _, _, SysFloc).

%% 

:- table get_system_by_floc/2.

% May return more than one result at the higher levels in the tree.
% Should be deterministic for plant and plant item.
get_system_by_floc(SaiCode, SysFloc) :- 
    aib_floc_to_s4_system(SaiCode, SysFloc).

get_system_by_floc(SaiCode, SysFloc) :- 
    aib_floc_below(SaiCode, Kid1),
    get_system_by_floc(Kid1, SysFloc).


%% 


aib_equipment_to_s4_system_floc(PliCode, SysFloc) :-
    s4_floc_l6_assembly(_, _, _, _, PliCode, SysFloc).

aib_equipment_to_s4_system_floc(PliCode, SysFloc) :-
    s4_floc_l7_item(_, _, _, _, _, PliCode, UFloc),
    s4_floc_l6_assembly(UFloc, _, _, _, _, SysFloc).

aib_equipment_to_s4_system(PliCode, Sys) :-
    s4_floc_l6_assembly(_, _, _, _, PliCode, SysFloc),
    s4_floc_l5_system(SysFloc, Sys, _, _).


aib_equipment_to_s4_system(PliCode, Sys) :-
    s4_floc_l7_item(_, _, _, _, _, PliCode, UFloc),
    s4_floc_l6_assembly(UFloc, _, _, _, _, SysFloc),
    s4_floc_l5_system(SysFloc, Sys, _, _).

%% 

:- table get_system_by_equipment/2.


get_system_by_equipment(PliCode, SysFloc) :- 
    aib_equipment_to_s4_system(PliCode, SysFloc).



%%% Test


% aib_asset_category('SAI00254010', X).
% aib_equipment_below('SAI00254010', X).

% aib_asset_category('AFL00329326', X).
% aib_equipment_below('AFL00329326', X).

% aib_asset_category('SAI00396905', X).
% aib_equipment_below('SAI00396905', X).


% import length/2 from basics.
% setof(X, aib_floc_below('SAI00003608', X), Xs), length(Xs, Len).


%% conversion

% For s4 levels 1-4 do we rely on conversion rules (potentially going
% out of date, incomplete, etc.) or data (potentially incomplete, not 
% currently accessible)?

% In short term use coversion rules...


% Sai should be installation
aib_ref_to_s4_floc(Sai, L1, L2) :- 
    aib_inst_floc1_floc2(Sai, L1, L2).

% Sai should be process group
aib_ref_to_s4_floc(Sai, L1, L2, L3) :- 
    aib_abs_floc(Sai, AibName1, AibName2, AibName3),
    aib_installation_sai_name(S1, AibName1),
    aib_inst_floc1_floc2(S1, L1, L2),
    % WARNING - temporarily don't match L2 to avoid rule overlap problems in the initial data
    aib_stype_procg_s4_fun_procg(AibName2, AibName3, _L2, L3).  

% Sai should be process
aib_ref_to_s4_floc(Sai, L1, L2, L3, L4) :- 
    aib_abs_floc(Sai, AibName1, AibName2, AibName3, AibName4),
    aib_installation_sai_name(S1, AibName1),
    aib_inst_floc1_floc2(S1, L1, L2),
    % WARNING - temporarily don't match L2 to avoid rule overlap problems in the initial data
    aib_stype_procg_proc_s4_fun_procg_proc(AibName2, AibName3, AibName4, _L2, L3, L4).

% aib_ref_to_s4_floc('SAI00003608', L1, L2).
% aib_ref_to_s4_floc('SAI00167636', L1, L2, L3).
% aib_ref_to_s4_floc('SAI00338989', L1, L2, L3, L4).

% Sai should be plant
% Note aib_ref_to_s4_floc/6 is just a degenerate case of aib_ref_to_s4_floc/7
aib_ref_to_s4_floc(Sai, L1, L2, L3, L4, L5) :- 
    aib_ref_to_s4_floc(Sai, L1, L2, L3, L4, L5, _).

% Sai should be plant
aib_ref_to_s4_floc(Sai, L1, L2, L3, L4, L5, L6) :- 
    aib_abs_floc(Sai, AibName1, AibName2, AibName3, AibName4, _AibName5),
    writeln([AibName2, AibName3, AibName4]),
    aib_installation_sai_name(S1, AibName1),
    aib_inst_floc1_floc2(S1, L1, L2),
    % WARNING - temporarily don't match L2 to avoid rule overlap problems in the initial data
    aib_stype_procg_proc_s4_fun_procg_proc(AibName2, AibName3, AibName4, _L2, L3, L4),
    aib_equipment_below(Sai, P1),
    aib_equipment_to_s4_system(P1, L5), 
    s4_floc_l6_assembly(_, L6, _, Sai, _, _).


% Note only a select number of sai numbers are mapped as the mapping is handcoded at the moment.

% aib_ref_to_s4_floc('SAI00253999', L1, L2, L3, L4, L5, L6).


% Sai should be plant item
aib_ref_to_s4_floc(Sai, L1, L2, L3, L4, L5, L6, L7) :- 
    aib_abs_floc(Sai, AibName1, AibName2, AibName3, AibName4, _AibName5),
    writeln([AibName2, AibName3, AibName4]),
    aib_installation_sai_name(S1, AibName1),
    aib_inst_floc1_floc2(S1, L1, L2),
    % WARNING - temporarily don't match L2 to avoid rule overlap problems in the initial data
    aib_stype_procg_proc_s4_fun_procg_proc(AibName2, AibName3, AibName4, _L2, L3, L4),
    aib_equipment_below(Sai, P1),
    aib_equipment_to_s4_system(P1, L5), 
    s4_floc_l7_item(_, L7, _, Sai, _, _, _),
    aib_floc_l6_plant_item(Sai, _,_,_, S2),
    s4_floc_l6_assembly(_, L6, _, S2, _, _).

% aib_ref_to_s4_floc('SAI00131528', L1, L2, L3, L4, L5, L6, L7).