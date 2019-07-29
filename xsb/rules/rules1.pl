% rules1,pl

% ['facts/aib_asset_category.pl', 'facts/aib_equipment.pl', 'facts/aib_floc_l1_l2_installation.pl'].
% ['facts/aib_floc_l3_process_group.pl', 'facts/aib_floc_l4_process.pl', 'facts/aib_floc_l5_plant.pl', 'facts/aib_floc_l6_plant_item.pl'].

% ['facts/s4_floc_l1_installation.pl', 'facts/s4_floc_l2_function.pl', 'facts/s4_floc_l3_process_group.pl', 'facts/s4_floc_l4_process.pl'].
% ['facts/s4_floc_l5_system.pl', 'facts/s4_floc_l6_unit.pl', 'facts/s4_floc_l7_subunit.pl'].

parent_process_group(X, Procg) :- 
    aib_floc_l4_process(X, _, _, _, Procg),
    aib_floc_l3_process_group(Procg, _, _, _, _).



:- table singular_plant_item/1.
singular_plant_item(X) :- 
    aib_floc_l6_plant_item(X, _, _, _, Y), tnot(singular_plant_item(Y)).

recursive_plant_item(X) :- 
    aib_floc_l6_plant_item(X, _, _, _, Y),
    aib_floc_l6_plant_item(Y, _, _, _, _).

aib_x3_x6(Procg, Proc, Plant, PlantItem) :-
    aib_floc_l3_process_group(Procg, _, _, _, _),
    aib_floc_l4_process(Proc, _, _, _, Procg),
    aib_floc_l5_plant(Plant, _, _, _, Proc),
    aib_floc_l6_plant_item(PlantItem, _, _, _, _).

aib_x3_x5(X3, X3Name, X4, X4Name, X5, X5Name) :-
    aib_floc_l3_process_group(X3, _, X3Name, _, _),
    aib_floc_l4_process(X4, _, X4Name, _, X3),
    aib_floc_l5_plant(X5, _, X5Name, _, X4).

aib_x3_x5(X3, X3Name, X4, X4Name, X5, X5Name) :-
    aib_floc_l3_process_group(X3, _, X3Name, _, _),
    aib_floc_l5_plant(X5, _, X5Name, _, X3),
    X4 = '',
    X4Name = ''.  


aib_x3_x4(X3, X3Name, X4, X4Name) :-
    aib_floc_l3_process_group(X3, _, X3Name, _, _),
    aib_floc_l4_process(X4, _, X4Name, _, X3).     


aib_x3_x4(X3, X3Name, X4, X4Name) :-
    aib_floc_l4_process(X4, _, X4Name, _, Parent),
    aib_asset_category(Parent, 'INSTALLATION'),
    X3 = '',
    X3Name = ''.
    

% Should we impose a virtual hierarchy with potentially empty process groups and plant / plant items?
% yes
