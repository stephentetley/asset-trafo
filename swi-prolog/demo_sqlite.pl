% demo_sqlite.pl

:- use_module(library(prosqlite)).
:- use_module(library(db_facts)).

%% sqlite_connect('sample_data/assets.sqlite', assets).

row_to_aib_installation(row(X,Y,Z), Ans) :-
    Ans = aib_installation(X,Y,Z).


demo01(Xs) :- 
    sqlite_connect('sample_data/assets.sqlite', assets),
    Sel = 'SELECT * FROM aib_installation',
    findall( X, (sqlite_query(assets, Sel, R), row_to_aib_installation(R,X)), Xs),
    sqlite_disconnect('assets').

demo02(Xs) :- 
    sqlite_connect('sample_data/assets.sqlite', assets),
    findall( X, db_holds(assets, aib_installation([ref=X, inst_type='SPS'])), Xs),
    sqlite_disconnect('assets').

demo02a(Xs) :- 
    sqlite_connect('sample_data/assets.sqlite', assets),
    findall( X, db_holds(aib_installation([name=X, inst_type='SPS'])), Xs),
    sqlite_disconnect('assets').