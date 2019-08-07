% demo_sqlite.pl

:- use_module(library(prosqlite)).
:- use_module(library(db_facts)).

%% sqlite_connect('data/assets.sqlite', assets).

row_to_aib_installation(row(X,Y,Z), Ans) :-
    Ans = aib_installation(X,Y,Z).


demo01(Xs) :- 
    sqlite_connect('data/assets.sqlite', assets),
    Sel = 'SELECT * FROM aib_installation',
    findall( X, (sqlite_query(assets, Sel, R), row_to_aib_installation(R,X)), Xs),
    sqlite_disconnect('assets').

demo02(Xs) :- 
    sqlite_connect('data/assets.sqlite', assets),
    findall( X, db_holds(assets, s4_equipment([s4_ref=X, category='I'])), Xs),
    sqlite_disconnect('assets').

demo02a(Xs) :- 
    sqlite_connect('sample_data/assets.sqlite', assets),
    findall( X, db_holds(s4_equipment([s4_ref=X, category='I'])), Xs),
    sqlite_disconnect('assets').