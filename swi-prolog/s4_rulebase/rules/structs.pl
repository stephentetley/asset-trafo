/*
    structs.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/  

:- module(structs, 
    [ 
        % Site
      get_s4_site/2
    , is_s4_site/1
    , s4_site_floc/2
    , s4_site_name/2
    , s4_site_child_function/2
    , s4_site_child_function_all/2

        % Function
    , is_s4_function/1
    , get_s4_function/2
    , s4_function_floc/2
    , s4_function_name/2    
    , s4_function_code/2
    , s4_function_object_description/2
    , s4_function_child_process_group/2
    , s4_function_child_process_group_all/2
    , s4_function_parent/2


        % ProcessGroup
    , get_s4_process_group/2
    , is_s4_process_group/1
    , s4_process_group_floc/2
    , s4_process_group_name/2    
    , s4_process_group_code/2    
    , s4_process_group_child_process/2
    , s4_process_group_child_process_all/2
    , s4_process_group_parent/2

        % Process
    , get_s4_process/2
    , is_s4_process/1
    , s4_process_floc/2
    , s4_process_name/2    
    , s4_process_code/2
    , s4_process_child_system/2
    , s4_process_child_system_all/2
    , s4_process_parent/2

        % System
    , get_s4_system/2
    , is_s4_system/1
    , s4_system_floc/2
    , s4_system_name/2    
    , s4_system_code/2
    , s4_system_child_assembly/2
    , s4_system_child_assembly_all/2
    , s4_system_child_equipment/2
    , s4_system_child_equipment_all/2
    , s4_process_parent/2

        % Assembly
    , get_s4_assembly/2
    , is_s4_assembly/1
    , s4_assembly_floc/2
    , s4_assembly_name/2    
    , s4_assembly_code/2
    , s4_assembly_child_equipment/2
    , s4_assembly_child_equipment_all/2
    , s4_assembly_parent/2

        % Equipment
    , get_s4_equipment/2

    ]).

%% Note - these are flat structs 
%% The tree shape is maintained by 'pointers' to the item's parent
%% rather than the parent having a collection of children.


:- use_module(library(record)).

:- record s4_site(floc:atom, name:atom).
:- record s4_function(floc:atom, name:atom, code:atom, object_description:atom, parent_ref:atom).
:- record s4_process_group(floc:atom, name:atom, code:atom, object_description:atom, parent_ref:atom).
:- record s4_process(floc:atom, name:atom, code:atom, object_description:atom, parent_ref:atom).
:- record s4_system(floc:atom, name:atom, code:atom, object_description:atom, 
        class_code:atom, class_description:atom, system_code:atom, parent_ref:atom).

:- record s4_assembly(floc:atom, name:atom, code:atom, object_description:atom, 
        class_code:atom, class_description:atom, parent_ref:atom).


:- record s4_equipment(uid:any, description:atom, category:atom, 
        object_type:atom, object_class:atom, s4_floc:atom).

%% Site

get_s4_site(Floc, Site) :-
    db_rules:s4_site(['s4_floc'=Floc, 'site_name'=Name]),
    make_s4_site([floc(Floc), name(Name)], Site).

s4_site_child_function(Site, Function) :-
    s4_site_floc(Site, F1),
    db_rules:s4_function(['s4_floc'=F2, 'parent_floc'=F1]),
    get_s4_function(F2, Function).

s4_site_child_function_all(Site, Functions) :-
    findall(X, s4_site_child_function(Site,X), Functions).

% site has no parent


%% Function

get_s4_function(Floc, Function) :- 
    db_rules:s4_function(['s4_floc'=Floc, 'function_name'=Name,
        'short_code'=Code, 'object_description'=ObjDesc, 
        'parent_floc'=Parent]),
    make_s4_function([floc(Floc), name(Name), code(Code), 
        object_description(ObjDesc), parent_ref(Parent)], Function).


s4_function_child_process_group(Function, ProcessGroup) :-
    s4_process_group_floc(Function, F1),
    db_rules:s4_process_group(['s4_floc'=F2, 'parent_floc'=F1]),
    get_s4_process_group(F2, ProcessGroup).

s4_function_child_process_group_all(Function, ProcessGroups) :-
    findall(X, s4_function_child_process_group(Function,X), ProcessGroups).

s4_function_parent(Function, Site) :-
    s4_function_parent_ref(Function, F1),
    get_s4_site(F1, Site).

%% ProcessGroup

get_s4_process_group(Floc, ProcessGroup) :-
    db_rules:s4_process_group(['s4_floc'=Floc, 'process_group_name'=Name,
        'short_code'=Code, 'object_description'=ObjDesc, 
        'parent_floc'=Parent]),
    make_s4_process_group([floc(Floc), name(Name), code(Code), 
        object_description(ObjDesc), parent_ref(Parent)], ProcessGroup).



s4_process_group_child_process(ProcessGroup, Process) :-
    s4_process_group_floc(ProcessGroup, F1),
    db_rules:s4_process(['s4_floc'=F2, 'parent_floc'=F1]),
    get_s4_process(F2, Process).

s4_process_group_child_process_all(ProcessGroup, Processes) :-
    findall(X, s4_process_group_process(ProcessGroup,X), Processes).


s4_process_group_parent(ProcessGroup, Function) :-
    s4_process_group_parent_ref(ProcessGroup, F1),
    get_s4_function(F1, Function).


%% Process

get_s4_process(Floc, Process) :-
    db_rules:s4_process(['s4_floc'=Floc, 'process_name'=Name,
        'short_code'=Code, 'object_description'=ObjDesc, 
        'parent_floc'=Parent]),
    make_s4_process_group([floc(Floc), name(Name), code(Code), 
        object_description(ObjDesc), parent(Parent)], Process).



s4_process_child_system(Process, System) :-
    s4_process_floc(Process, F1),
    db_rules:s4_system(['s4_floc'=F2, 'parent_floc'=F1]),
    get_s4_system(F2, System).

s4_process_child_system_all(Process, Systems) :-
    findall(X, s4_process_child_system(Process,X), Systems).


s4_process_parent(Process, ProcessGroup) :-
    s4_process_parent_ref(Process, F1),
    get_s4_process_group(F1, ProcessGroup).


%% System

get_s4_system(Floc, System) :-
    db_rules:s4_system(['s4_floc'=Floc, 'system_name'=Name,
            'object_code'=Code, 'object_description'=ObjDesc, 
            'class_code'=ClassCode, 'class_description'=ClassDesc, 
            'system_code'=SystemCode, 'parent_floc'=Parent]),    
    make_s4_system([floc(Floc), name(Name), code(Code), 
        object_description(ObjDesc), class_code(ClassCode), 
        class_description(ClassDesc), system_code(SystemCode), 
        parent_ref(Parent)], System).


s4_system_child_assembly(System, Assembly) :-
    s4_system_floc(System, F1),
    db_rules:s4_assembly(['s4_floc'=F2, 'parent_floc'=F1]),
    get_s4_assembly(F2, Assembly).

s4_system_child_assembly_all(System, Assemblies) :-
    findall(X, s4_system_child_assembly(System,X), Assemblies).


s4_system_child_equipment(System, Equipment) :-
    s4_system_floc(System, F1),
    db_rules:s4_equipment(['s4_equip_ref'=X, 's4_floc'=F1]),
    get_s4_equipment(X, Equipment).


s4_system_child_equipment_all(System, Equipments) :-
    findall(X, s4_system_child_equipment(System,X), Equipments).


s4_system_parent(System, Process) :-
    s4_system_parent_ref(System, F1),
    get_s4_process(F1, Process).



%% Assembly

get_s4_assembly(Floc, System) :-
    db_rules:s4_assembly(['s4_floc'=Floc, 'assembly_name'=Name,
            'object_code'=Code, 'object_description'=ObjDesc, 
            'class_code'=ClassCode, 'class_description'=ClassDesc, 
            'parent_floc'=Parent]),    
    make_s4_assembly([floc(Floc), name(Name), code(Code), 
        object_description(ObjDesc), class_code(ClassCode), 
        class_description(ClassDesc), 
        parent_ref(Parent)], System).



s4_assembly_child_equipment(Assembly, Equipment) :-
    s4_assembly_floc(Assembly, F1),
    db_rules:s4_equipment(['s4_equip_ref'=X, 's4_floc'=F1]),
    get_s4_equipment(X, Equipment).


s4_assembly_child_equipment_all(Assembly, Equipments) :-
    findall(X, s4_assembly_child_equipment(Assembly,X), Equipments).

s4_assembly_parent(Assembly, System) :-
    s4_assembly_parent_ref(Assembly, F1),
    get_s4_system(F1, System).


%% Equipment

get_s4_equipment(Uid, Equipment) :-
    db_rules:s4_equipment(['s4_equip_ref'=Uid, 'description'=Descr,
            'category'=Category, 'object_type'=ObjType, 
            'object_class'=ObjClass, 's4_floc'=S4Floc]),              
    make_s4_equipment([uid(Uid), description(Descr), category(Category), 
        object_type(ObjType), object_class(ObjClass),         
        s4_floc(S4Floc)], Equipment).
