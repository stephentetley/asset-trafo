% demo04.pl

:- use_module(library(prosqlite)).

:- use_module(rules/structs).

db_connect :- 
    sqlite_connect('data/db/s4_rulebase.sqlite', 
        rulebase,
        [as_predicates(true), arity(palette), at_module(db_rules)]).

db_disconnect :- 
    sqlite_disconnect(rulebase).


demo01 :- 
    atom("string").

demo02(Site, Funs) :- 
    get_s4_site('SEAME', Site),
    s4_site_function_all(Site, Funs).

demo03(SiteFloc, Ans) :- 
    get_s4_site(SiteFloc, Site),
    s4_site_child_function(Site, Fun), 
    s4_function_code(Fun, 'EDC'),
    s4_function_parent(Fun, Ans).

