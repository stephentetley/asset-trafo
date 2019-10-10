/*
    rules.pl
    Copyright (c) Stephen Tetley 2019
    License: BSD 3 Clause
*/    

:- module(rules, 
    [ is_site/1
    , site_name/2  
    , site_functions/2
    ]).

%% Test is site exists
is_site(Code) :-
    db_rules:s4_site(['s4_floc'=Code, 'site_name'=_]),!.

site_name(Code, Name) :-
    db_rules:s4_site(['s4_floc'=Code, 'site_name'=Name]).

site_functions(Code, FunCodes) :- 
    findall(X, db_rules:s4_function('parent_floc'=Code, 's4_floc'=X), FunCodes).