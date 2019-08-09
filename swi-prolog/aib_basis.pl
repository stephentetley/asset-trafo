% aib_basis.pl


:- module(aib_basis,
    [ is_aib_installation/1
    , is_aib_process_group/1
    , is_aib_process/1
    , is_aib_plant/1
    , is_aib_plant_item/1
    , is_aib_equipment/1


    ]).

:- use_module(library(db_facts)).

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


%% aib_funlocs_below (Ref, Kids).

aib_installation(Ref) :- 
    db_holds(assets, aib_installation([sai_ref=Ref, common_name=_])), 
    !.

aib_installation_named(Name) :- 
    db_holds(assets, aib_installation([sai_ref=_, common_name=Name])), 
    !.