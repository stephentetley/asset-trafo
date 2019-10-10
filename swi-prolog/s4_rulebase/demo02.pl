% demo02.pl

:- use_module(library(prosqlite)).

%% prosqlite
%% connection opened with as_predicates
%% as_predicates generates a predicate for each table, usefully
%% we don't have to to pass the connection around to predicate calls

%% This could be useful alternative to FactX.

db_connect :- 
    sqlite_connect('data/db/s4_rulebase.sqlite', 
        rulebase,
        [as_predicates(true)]).

db_disconnect :- 
    sqlite_disconnect(rulebase).


demo01(Xs) :- 
    findall(X, s4_site(X,_), Xs).

demo02(X,Y) :- 
    s4_site(P1,X),
    s4_function(P2, _, _, _, P1),
    s4_process_group(P3, _, _, _, P2),
    s4_process(_, Y, _, _, P3).

demo03(E) :- 
    s4_equipment(_,E,_,_,_, 'SEAME-CAA-NET-TEL-SYS01').


