/*
    structs.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/  

:- module(structs, 
    [ is_s4_site/1
    , get_s4_site/2
    , s4_site_floc/2
    , s4_site_name/2
    , s4_site_function/2
    , s4_site_function_all/2
    , is_s4_function/1
    , s4_function_name/2
    ]).

%% Note - these are flat structs 
%% The tree shape is maintained by 'pointers' to the item's parent
%% rather than the parent having a collection of children.


:- use_module(library(record)).

:- record s4_site(floc:atom, name:atom).

get_s4_site(Floc, Site) :-
    db_rules:s4_site(['s4_floc'=Floc, 'site_name'=Name]),
    make_s4_site([floc(Floc), name(Name)], Site).

s4_site_function(Site, Function) :-
    s4_site_floc(Site, F1),
    db_rules:s4_function(['s4_floc'=F2, 'parent_floc'=F1]),
    get_s4_function(F2, Function).

s4_site_function_all(Site, Functions) :-
    findall(X, s4_site_function(Site,X), Functions).

:- record s4_function(floc:atom, name:atom, code:atom, object_description:atom, parent:atom).

get_s4_function(Floc, Function) :- 
    db_rules:s4_function(['s4_floc'=Floc, 'function_name'=Name,
        'short_code'=Code, 'object_description'=ObjDesc, 
        'parent_floc'=Parent]),
    make_s4_function([floc(Floc), name(Name), code(Code), 
        object_description(ObjDesc), parent(Parent)], Function).

