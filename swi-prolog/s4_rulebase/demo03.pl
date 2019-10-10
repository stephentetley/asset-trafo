% demo03.pl

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(yall)).

:- use_module(library(prosqlite)).
:- use_module(rules/rules).

%% prosqlite
%% connection opened with as_predicates
%% as_predicates generates a predicate for each table, usefully
%% we don't have to to pass the connection around to predicate calls

%% This could be useful alternative to FactX.

db_connect :- 
    sqlite_connect('data/db/s4_rulebase.sqlite', 
        rulebase,
        [as_predicates(true), arity(palette), at_module(db_rules)]).

db_disconnect :- 
    sqlite_disconnect(rulebase).


demo01(Code, Pgs) :- 
    is_site(Code), 
    site_function_all(Code, Xs), 
    convlist([X,Y] >> function_process_groups(X,Y), Xs, Yss),
    flatten(Yss, Pgs).


demo02(PgCode, Ps) :-
    process_group_processes(PgCode, Ps).

demo03(SysCode, Es) :-
    system_equipment(SysCode, Es).



