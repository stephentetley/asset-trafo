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
    , s4_site_function/2
    , s4_site_function_all/2

        % Function
    , is_s4_function/1
    , get_s4_function/2
    , s4_function_floc/2
    , s4_function_name/2    
    , s4_function_code/2
    , s4_function_object_description/2
    , s4_function_process_group/2
    , s4_function_process_group_all/2
    , s4_function_parent/2


        % ProcessGroup
    , get_s4_process_group/2
    , is_s4_process_group/1
    , s4_process_group_floc/2
    , s4_process_group_name/2    
    , s4_process_group_code/2
    , s4_process_group_parent/2

        % Process
    , get_s4_process/2
    , is_s4_process/1
    , s4_process_floc/2
    , s4_process_name/2    
    , s4_process_code/2

        % System
    , get_s4_system/2
    , is_s4_system/1

        % Assembly
    , get_s4_assembly/2
    , is_s4_assembly/1
    
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

%% Site

get_s4_site(Floc, Site) :-
    db_rules:s4_site(['s4_floc'=Floc, 'site_name'=Name]),
    make_s4_site([floc(Floc), name(Name)], Site).

s4_site_function(Site, Function) :-
    s4_site_floc(Site, F1),
    db_rules:s4_function(['s4_floc'=F2, 'parent_floc'=F1]),
    get_s4_function(F2, Function).

s4_site_function_all(Site, Functions) :-
    findall(X, s4_site_function(Site,X), Functions).

%% Function

get_s4_function(Floc, Function) :- 
    db_rules:s4_function(['s4_floc'=Floc, 'function_name'=Name,
        'short_code'=Code, 'object_description'=ObjDesc, 
        'parent_floc'=Parent]),
    make_s4_function([floc(Floc), name(Name), code(Code), 
        object_description(ObjDesc), parent_ref(Parent)], Function).


s4_function_process_group(Site, ProcessGroup) :-
    s4_site_floc(Site, F1),
    db_rules:s4_process_group(['s4_floc'=F2, 'parent_floc'=F1]),
    get_s4_process_group(F2, ProcessGroup).

s4_function_process_group_all(Site, ProcessGroups) :-
    findall(X, s4_function_process_group(Site,X), ProcessGroups).

s4_function_parent(Function, Site) :-
    s4_function_floc(Function, F1),
    db_rules:s4_function(['s4_floc'=F1, 'parent_floc'=F2]),
    get_s4_site(F2, Site).

%% ProcessGroup

get_s4_process_group(Floc, ProcessGroup) :-
    db_rules:s4_process_group(['s4_floc'=Floc, 'process_group_name'=Name,
        'short_code'=Code, 'object_description'=ObjDesc, 
        'parent_floc'=Parent]),
    make_s4_process_group([floc(Floc), name(Name), code(Code), 
        object_description(ObjDesc), parent_ref(Parent)], ProcessGroup).



s4_process_group_process(Site, Process) :-
    s4_site_floc(Site, F1),
    db_rules:s4_process(['s4_floc'=F2, 'parent_floc'=F1]),
    get_s4_process(F2, Process).

s4_process_group_process_all(Site, Processes) :-
    findall(X, s4_process_group_process(Site,X), Processes).


s4_process_group_parent(ProcessGroup, Function) :-
    s4_process_group_floc(ProcessGroup, F1),
    db_rules:s4_process_group(['s4_floc'=F1, 'parent_floc'=F2]),
    get_s4_function(F2, Function).


%% Process

get_s4_process(Floc, Process) :-
    db_rules:s4_process(['s4_floc'=Floc, 'process_name'=Name,
        'short_code'=Code, 'object_description'=ObjDesc, 
        'parent_floc'=Parent]),
    make_s4_process_group([floc(Floc), name(Name), code(Code), 
        object_description(ObjDesc), parent(Parent)], Process).



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
