% translate_floc2.pl

:- module(translate_floc2,
    [ atoms_to_floc/2
    , floc_take/3

    , is_aib_installation/1
    , is_aib_process_group/1
    , is_aib_process/1
    , is_aib_plant/1
    , is_aib_plant_item/1
    , is_aib_equipment/1

    , aib_funloc_below/2
    , aib_equipment_below/2

    , aib_ref_to_s4_floc/2
    ]).

:- use_module(library(prosqlite)).
:- use_module(library(db_facts)).


%%% Manipulate s4 FLOC symbols

atoms_to_floc(List, Floc) :- 
    atomics_to_string(List, "-", S1),
    string_to_atom(S1, Floc).
    

% if count > len take whole list.
list_take(Src, Count, List) :- 
    findall(Elt, (nth1(Ix, Src, Elt), Ix =< Count), List).

floc_take(Src, Count, Floc) :- 
    atomics_to_string(List, "-", Src),
    list_take(List, Count, List1),
    atoms_to_floc(List1, Floc).


%%% 

is_aib_installation(Ref) :- 
    db_holds(assets, aib_installation([sai_ref=Ref, common_name=_])), 
    !.

is_aib_process_group(Ref) :- 
    db_holds(assets, aib_process_group([sai_ref=Ref, asset_name=_])),
    !.

is_aib_process(Ref) :- 
    db_holds(assets, aib_process([sai_ref=Ref, asset_name=_])),
    !.

is_aib_plant(Ref) :- 
    db_holds(assets, aib_plant([sai_ref=Ref, asset_name=_])),
    !.

is_aib_plant_item(Ref) :- 
    db_holds(assets, aib_plant_item([sai_ref=Ref, asset_name=_])),
    !.

is_aib_equipment(Ref) :- 
    db_holds(assets, aib_equipment([pli_ref=Ref, equipment_name=_])),
    !.


%%

%% Above plant_item
aib_funloc_below(Parent, Child) :-
    db_holds(assets, aib_plant_item([sai_ref=Child, parent_ref=Parent])).

%% One above plant
aib_funloc_below(Parent, Child) :-
    db_holds(assets, aib_plant([sai_ref=Child, parent_ref=Parent])). 


%% Above-above plant
aib_funloc_below(Parent, Child) :-
    db_holds(assets, aib_plant([sai_ref=X1, parent_ref=Parent])), 
    aib_funloc_below(X1, Child).     

% One above process
aib_funloc_below(Parent, Child) :-
    db_holds(assets, aib_process([sai_ref=Child, parent_ref=Parent])).


%% Above-above process
aib_funloc_below(Parent, Child) :-
    db_holds(assets, aib_process([sai_ref=X1, parent_ref=Parent])),
    aib_funloc_below(X1, Child).


%% One above process_group
aib_funloc_below(Parent, Child) :-
    db_holds(assets, aib_process_group([sai_ref=Child, parent_ref=Parent])).

%% Above-above process_group
aib_funloc_below(Parent, Child) :-
    db_holds(assets, aib_process_group([sai_ref=X1, parent_ref=Parent])),
    aib_funloc_below(X1, Child).  


% This case should capture aib_plant_item and any other type
% with directly attached equipment.
aib_equipment_below(SaiCode, PliCode) :- 
    db_holds(assets, aib_equipment([ pli_ref= PliCode, parent_ref=SaiCode])).

aib_equipment_below(SaiCode, PliCode) :- 
    aib_funloc_below(SaiCode, Kid1),
    aib_equipment_below(Kid1, PliCode).



aib_equipment_to_s4_floc(PliCode, Floc) :- 
    db_holds(assets, s4_equipment([ pli_code=PliCode, s4_floc=Floc])).


%% aib_ref_to_s4_floc_aux

% Installation retrieves a length 1 floc (site)
% This is two slow as it finds all kids - it should be a 
% special case that short circuits...
aib_ref_to_s4_floc_aux(Ref, Floc) :- 
    is_aib_installation(Ref),
    aib_equipment_below(Ref, Equip), 
    aib_equipment_to_s4_floc(Equip, Floc1),
    floc_take(Floc1, 1, Floc).

% Process_group retrieves a length 3 floc (process group)
aib_ref_to_s4_floc_aux(Ref, Floc) :- 
    is_aib_process_group(Ref),
    aib_equipment_below(Ref, Equip), 
    aib_equipment_to_s4_floc(Equip, Floc1),
    floc_take(Floc1, 3, Floc).

% process retrieves a length 4 floc (process)
aib_ref_to_s4_floc_aux(Ref, Floc) :- 
    is_aib_process(Ref),
    aib_equipment_below(Ref, Equip), 
    aib_equipment_to_s4_floc(Equip, Floc1),
    floc_take(Floc1, 4, Floc).

% Plant retrieves a length 6 floc (assembly)
aib_ref_to_s4_floc_aux(Ref, Floc) :- 
    is_aib_plant(Ref),
    aib_equipment_below(Ref, Equip), 
    aib_equipment_to_s4_floc(Equip, Floc1),
    floc_take(Floc1, 6, Floc).

% Plant_item retrieves a length 7 floc (item)
aib_ref_to_s4_floc_aux(Ref, Floc) :- 
    is_aib_plant_item(Ref),
    aib_equipment_below(Ref, Equip), 
    aib_equipment_to_s4_floc(Equip, Floc1),
    floc_take(Floc1, 7, Floc).

% Equipment floc is variable length (no truncation)
aib_ref_to_s4_floc_aux(Ref, Floc) :-
    aib_equipment_to_s4_floc(Ref, Floc).


aib_ref_to_s4_floc(Ref, Flocs) :- 
    setof(X, aib_ref_to_s4_floc_aux(Ref,X), Flocs).