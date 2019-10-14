/*
    select.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/  

:- module(select, 
    [ get_s4_site_by_name/2 
    , get_s4_site_by_re/2 

    , get_function/3
    , get_process_group/3
    , get_process/3
    , get_system/3
    , get_equipment/2

    ] ).

:- use_module(rules/structs).
:- use_module(library(pcre)).



get_s4_site_by_name(Name, Ans) :-
    get_s4_site(_, Ans),
    s4_site_name(Ans, Name).


get_s4_site_by_re(Patt, Ans) :-
    get_s4_site(_, Ans),
    s4_site_name(Ans, Name),
    re_match(Patt, Name).



get_function(Site, Code, Function) :- 
    is_s4_site(Site),
    s4_site_child_function(Site, Function),
    s4_function_code(Function, Code).

get_process_group(Function, Code, ProcessGroup) :- 
    is_s4_function(Function),
    s4_function_child_process_group(Function, ProcessGroup),
    s4_process_group_code(ProcessGroup, Code).

get_process(ProcessGroup, Code, Process) :-  
    is_s4_process_group(ProcessGroup),
    s4_process_group_child_process(ProcessGroup, Process),
    s4_process_code(Process, Code).

get_system(Process, Code, System) :- 
    is_s4_process(Process),
    s4_process_child_system(Process, System),
    s4_system_code(System, Code).


%% TODO - addressing?
get_equipment(System, Equipment) :- 
    s4_system_child_equipment(System, Equipment).




