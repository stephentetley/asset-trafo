% demo01.pl

:- use_module(library(prosqlite)).
:- use_module(library(pcre)).

:- use_module(rules/structs).
:- use_module(rules/select).


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
demo04(Ans) :-
    get_s4_site_by_name('Wistow WwTW', Ans).

%% Need a 'fuzzy' way of finding sites, etc...
%% PCRE to the rescue
demo05(Patt, Ans) :-
    get_s4_site_by_re(Patt, Ans).


demo06(Ans) :- 
    get_s4_site_by_re("^Selby", X1),
    get_function(X1, 'CAA', X2),
    get_process_group(X2, 'NET', X3),
    get_process(X3, 'TEL', X4),
    get_system(X4, 'SYS01', Ans).
    %% get_equipment(X5, Ans).


demo07_aux("One").
demo07_aux("Two").

failing(_) :- false.
id(X, X).

demo07z(Ans) :- 
    findall(X, demo07_aux(X), Ans).

demo07(Ans) :- 
    count(demo07_aux, Ans).

demo08(Ans) :- 
    get_s4_site_by_re("^Selby.*CSO\\Z", X1),
    count(get_function(X1, 'CAA'), Ans), 
    !.

demo09 :- 
    get_s4_site_by_re("^Selby.*CSO\\Z", X1),
    satisfies(get_function(X1, 'CAA')),
    !.

demo10(Ans) :-
    get_s4_site_by_re("^Selby.*CSO\\Z", X1),
    get_function(X1, X2),
    get_process_group(X2, Ans),
    s4_process_group_code(Ans, 'NET').


