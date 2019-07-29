% asset_sync.pl

% ['facts/aib_asset_category.pl', 'facts/aib_equipment.pl', 'facts/aib_floc_l1_l2_installation.pl'].
% ['facts/aib_floc_l3_process_group.pl', 'facts/aib_floc_l4_process.pl', 'facts/aib_floc_l5_plant.pl', 'facts/aib_floc_l6_plant_item.pl'].

% ['facts/s4_floc_l1_installation.pl', 'facts/s4_floc_l2_function.pl', 'facts/s4_floc_l3_process_group.pl', 'facts/s4_floc_l4_process.pl'].
% ['facts/s4_floc_l5_system.pl', 'facts/s4_floc_l6_unit.pl', 'facts/s4_floc_l7_subunit.pl'].

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
    s4_floc_l6_unit(_, _, SaiCode, _, SysFloc).

aib_floc_to_s4_system(SaiCode, SysFloc) :-
    s4_floc_l7_subunit(_, _, SaiCode, _, _, UFloc),
    s4_floc_l6_unit(UFloc, _, _, _, SysFloc).

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

aib_equipment_to_s4_system(PliCode, SysFloc) :-
    s4_floc_l6_unit(_, _, _, PliCode, SysFloc).

aib_equipment_to_s4_system(PliCode, SysFloc) :-
    s4_floc_l7_subunit(_, _, _, _, PliCode, UFloc),
    s4_floc_l6_unit(UFloc, _, _, _, SysFloc).

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
