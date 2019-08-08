% translate_floc.pl

:- module(translate_floc,
    [ atoms_to_floc/2
    , floc_take/3
    , floc_prefix/2

    , aib_funloc_below/2
    , aib_equipment_below/2


    , aib_ref_to_s4_floc/2
    , s4_floc_to_aib_ref/2
    ]).

:- use_module(library(prosqlite)).
:- use_module(library(db_facts)).

:- use_module(aib_basis).


%% # aib_ref_to_s4_floc/2
%%
%% This is a data-oriented translation bewteen function locations
%% for Aib and S4.
%% If the fingerprint data exists in both systems, we can translate 
%% functional locations from Aib to S4.
%% Sometimes the trnslate may generate more than one result, if 
%% children of the function location have been mapped to different
%% parts of the tree.





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
    db_holds(assets, s4_equipment([ aib_pli_code=PliCode, s4_floc=Floc])).


%% aib_ref_to_s4_floc_aux

% Installation retrieves a length 1 floc (site)
% We assume that all children of an installation have been mapped
% to the same site in S4, and hence we use a cut after we have found 
% the first child.
% For lower nodes a similar uniqueness is not guaranteed and we must
% return a set of successes.
aib_ref_to_s4_floc_aux(Ref, Floc) :- 
    is_aib_installation(Ref),
    aib_equipment_below(Ref, Equip), 
    !,      % Cut at first equipment.
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

% This is not bi-directional!
aib_ref_to_s4_floc(Ref, Flocs) :- 
    setof(X, aib_ref_to_s4_floc_aux(Ref,X), Flocs).





%%% s4_floc_to_aib_ref/2

%% The reverse translation works on the S4 objects always having 
%% a reference to the Aib Sai number. 
%% This assumption may be overreaching.

s4_floc_to_aib_ref(Floc, Ref) :- 
    db_holds(assets, s4_site([ s4_floc=Floc, aib_ref=Ref ])), 
    !.

s4_floc_to_aib_ref(Floc, Ref) :- 
    db_holds(assets, s4_function([ s4_floc=Floc, aib_ref=Ref ])), 
    !.

s4_floc_to_aib_ref(Floc, Ref) :- 
    db_holds(assets, s4_process_group([ s4_floc=Floc, aib_ref=Ref ])), 
    !.

s4_floc_to_aib_ref(Floc, Ref) :- 
    db_holds(assets, s4_process([ s4_floc=Floc, aib_ref=Ref ])),
    !.

s4_floc_to_aib_ref(Floc, Ref) :- 
    db_holds(assets, s4_system([ s4_floc=Floc, aib_ref=Ref ])),
    !.

s4_floc_to_aib_ref(Floc, Ref) :- 
    db_holds(assets, s4_assembly([ s4_floc=Floc, aib_ref=Ref ])), 
    !.

s4_floc_to_aib_ref(Floc, Ref) :- 
    db_holds(assets, s4_item([ s4_floc=Floc, aib_ref=Ref ])), 
    !.

s4_floc_to_aib_ref(Floc, Ref) :- 
    db_holds(assets, s4_component([ s4_floc=Floc, aib_ref=Ref ])),
    !.

%% Natural mapping for equipment.
%% 


s4_floc_to_aib_equipment(Floc, PliCode) :- 
    db_holds(assets, s4_equipment([ s4_floc=Floc, aib_pli_code=PliCode ])).

