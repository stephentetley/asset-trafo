


parent_process_group(X, Procg) :- 
    aib_process(X, _, _, _, Procg),
    aib_process_group(Procg, _, _, _, _).



:- table singular_plant_item/1.
singular_plant_item(X) :- 
    aib_plant_item(X, _, _, _, Y), tnot(singular_plant_item(Y)).

recursive_plant_item(X) :- 
    aib_plant_item(X, _, _, _, Y),
    aib_plant_item(Y, _, _, _, _).

aib_x3_x6(Procg, Proc, Plant, PlantItem) :-
    aib_process_group(Procg, _, _, _, _),
    aib_process(Proc, _, _, _, Procg),
    aib_plant(Plant, _, _, _, Proc),
    aib_plant_item(PlantItem, _, _, _, _).

aib_x3_x5(X3, X3Name, X4, X4Name, X5, X5Name) :-
    aib_process_group(X3, _, X3Name, _, _),
    aib_process(X4, _, X4Name, _, X3),
    aib_plant(X5, _, X5Name, _, X4).

aib_x3_x5(X3, X3Name, X4, X4Name, X5, X5Name) :-
    aib_process_group(X3, _, X3Name, _, _),
    aib_plant(X5, _, X5Name, _, X3),
    X4 = '',
    X4Name = ''.  

aib_x3_x4(X3, X3Name, X4, X4Name) :-
    aib_process_group(X3, _, X3Name, _, _),
    aib_process(X4, _, X4Name, _, X3).      

% Should we impose a virtual hierarchy with potentially empty process groups and plant / plant items?

