% demo03.pl

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


demo01(Code) :- 
    is_site(Code).

% demo02(Code) :- 
%     s4_site(Code, _), !.


% demo03(Name) :- 
%     site(Obj, 'SEAME'),
%     Obj = s4_site(_, Name), !.
