% demo04.pl

:- use_module(library(prosqlite)).
:- use_module(library(pcre)).

:- use_module(rules/structs).

db_connect :- 
    sqlite_connect('data/db/s4_rulebase.sqlite', 
        rulebase,
        [as_predicates(true), arity(palette), at_module(db_rules)]).

db_disconnect :- 
    sqlite_disconnect(rulebase).


demo01 :- 
    atom("string").

demo02(SiteFloc, Funs) :- 
    get_s4_site(SiteFloc, Site),
    s4_site_child_function_all(Site, Funs).


%% Site with an environmental discharge...
demo03(SiteFloc, Ans) :- 
    get_s4_site(SiteFloc, Site),
    s4_site_child_function(Site, Fun), 
    s4_function_code(Fun, 'EDC'),
    s4_function_parent(Fun, Ans).

%% Need a 'fuzzy' way of finding sites, etc...
%% PCRE to the rescue
demo04(Patt, Ans) :-
    get_s4_site(SiteFloc, Ans),
    s4_site_name(Ans, Name),
    re_match(Patt, Name).


