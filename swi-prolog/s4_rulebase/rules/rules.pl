/*
    rules.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/    

:- module(rules, 
    [ is_site/1
    , site_name/2  
    , site_function/2
    , site_function_all/2
    , is_function/1
    , function_name/2  
    , function_process_groups/2
    , is_process_group/1
    , process_group_name/2  
    , process_group_processes/2
    , is_process/1
    , process_name/2  
    , process_systems/2
    , is_system/1
    , system_name/2  
    , system_object_code/2
    , system_assemblies/2
    , system_equipment/2
    ]).


%%% Obsolete
%%% It is nicer to return a record *that can be deconstructed)
%%% than a uid that has to be chined into another query.

%% Site
is_site(Code) :-
    db_rules:s4_site(['s4_floc'=Code, 'site_name'=_]),!.

site_name(Code, Name) :-
    db_rules:s4_site(['s4_floc'=Code, 'site_name'=Name]).

site_function(Code, Kid) :- 
    db_rules:s4_function('parent_floc'=Code, 's4_floc'=Kid).

site_function_all(Code, FunCodes) :- 
    findall(X, db_rules:s4_function('parent_floc'=Code, 's4_floc'=X), FunCodes).

%% Function 
is_function(Code) :-
    db_rules:s4_function(['s4_floc'=Code, 'function_name'=_]),!.

function_name(Code, Name) :-
    db_rules:s4_function(['s4_floc'=Code, 'function_name'=Name]).

function_process_groups(Code, Kids) :- 
    findall(X, db_rules:s4_process_group('parent_floc'=Code, 's4_floc'=X), Kids).

%% ProcessGroup
is_process_group(Code) :-
    db_rules:s4_process_group(['s4_floc'=Code, 'process_group_name'=_]),!.

process_group_name(Code, Name) :-
    db_rules:s4_process_group(['s4_floc'=Code, 'process_group_name'=Name]).

process_group_processes(Code, Kids) :- 
    findall(X, db_rules:s4_process('parent_floc'=Code, 's4_floc'=X), Kids).

%% Process 
is_process(Code) :-
    db_rules:s4_process(['s4_floc'=Code, 'process_name'=_]),!.

process_name(Code, Name) :-
    db_rules:s4_process(['s4_floc'=Code, 'process_name'=Name]).

process_systems(Code, Kids) :- 
    findall(X, db_rules:s4_system('parent_floc'=Code, 's4_floc'=X), Kids).


%% System
is_system(Code) :-
    db_rules:s4_system(['s4_floc'=Code, 'process_name'=_]),!.

system_name(Code, Name) :-
    db_rules:s4_system(['s4_floc'=Code, 'process_name'=Name]).

system_object_code(Floc, ObjCode) :- 
    db_rules:s4_system(['s4_floc'=Floc, 'object_code'=ObjCode]).

system_assemblies(Code, Kids) :- 
    findall(X, db_rules:s4_assembly('parent_floc'=Code, 's4_floc'=X), Kids).

system_equipment(Code, Kids) :- 
    findall(X, db_rules:s4_equipment('s4_floc'=Code, 's4_equip_ref'=X), Kids).