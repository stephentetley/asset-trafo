% demo02.pl

:- use_module(library(prosqlite)).
:- use_module(library(db_facts)).

:- use_module(s4_basis).
:- use_module(translate_floc).

%% Before testing the demos at the prompt...
%% ?-  assets_connect.

%% When done
%% ?- assets_disconnect.

assets_connect :- 
    sqlite_connect('data/assets.sqlite', assets).

assets_disconnect :- 
    db_disconnect(assets).


temp01(S) :- 
    atoms_to_floc(['ABB01', 'WTN'], S).

temp02(S) :- 
    floc_take('ABB01-WTN-WTF-PMG-SYS01-PMP01', 3, S).

demo01(Xs) :- 
    findall(X, aib_funloc_below('SAI00162176', X), Xs).

demo02(Xs) :- 
    findall(X, aib_funloc_below('SAI00130369', X), Xs).

demo03(Xs) :- 
    findall(X, aib_funloc_below('SAI00130367', X), Xs).

demo04(Xs) :- 
    % Note this is slooowwww...
    findall(X, aib_funloc_below('SAI00002341', X), Xs).

demo05(Xs) :- 
    findall(X, aib_equipment_below('SAI00130367', X), Xs).

demo06(Xs) :- 
    aib_ref_to_s4_floc('PLI00317969', Xs).


demo07(Xs) :- 
    %% Plant 'TELEMETRY OUTSTATION'
    aib_ref_to_s4_floc('SAI00162177', Xs).


demo08(Xs) :- 
    %% Process 'RTS MONITORING'
    aib_ref_to_s4_floc('SAI00130369', Xs).


demo09(Xs) :- 
    %% Process Group 'CONTROL SERVICES'
    %% This appears to have a valid reason for returning multiple 
    %% results - children in Aib have been moved under different
    %% (more than 1) process groups in S4.
    aib_ref_to_s4_floc('SAI00130367', Xs).


demo10(Xs) :- 
    %% Installation ...
    aib_ref_to_s4_floc('SAI00002341', Xs).

demo10a(X) :- 
    s4_floc_to_aib_ref('ALDWK', X).

demo20(Xs) :- 
    aib_ref_to_s4_floc('SAI00002341', Xs).

demo20a :-
    % should be true
    is_aib_installation('SAI00002341').

demo20b(Ref) :- 
    db_holds(assets, aib_installation([sai_ref=Ref, common_name=_])),
    !.

