% demo01.pl

:- use_module(library(prosqlite)).

%% prosqlite
%% connection opened with as_predicates


%% Before testing the demos at the prompt...
%% ?-  assets_connect.

%% When done
%% ?- assets_disconnect.

db_connect :- 
    sqlite_connect('./data/db/s4_rulebase.sqlite', rulebase), !.

db_disconnect :- 
    sqlite_disconnect(rulebase).


%% Returns a "Prolog answer" (one record at once, iteratable with semicolon)
%% rather than a set.
demo01(Row) :- 
    sqlite_query(rulebase, "SELECT site.s4_floc FROM s4_site AS site;", Row).



