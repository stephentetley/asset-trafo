% demo01.pl

:- use_module(floc_rules/floc_rule_mapping_1_2).
:- use_module(floc_rules/floc_rule_mapping_2_3).
:- use_module(translate_floc).

:- use_module(sample_data/aib_inst_facts1).
:- use_module(sample_data/s4_site_facts1).


demo01(Floc) :- 
    aib_installation_to_s4_floc('SAI00003608', Floc).

demo02(X,Y) :- 
    aib_installation(Sai, 'ABBEY LANE HULL/SPS', _),
    aib_inst_floc1_s4_name(Sai, X, Y).

demo03(Inst) :-
    process_group_installation_root('SAI00075549', Inst).

find_process_group(Sai, Ans) :- 
    aib_process_group(Sai, X, Y, Z),
    Ans = aib_process_group(Sai, X, Y, Z).

demo03a(Body) :-
    find_process_group('SAI00167635', Body).


demo04(Inst) :-
    process_installation_root('SAI00338989', Inst).

demo04a(Inst) :-
    process_installation_root('SAI00075544', Inst).


demo05(Floc) :- 
    aib_process_group_to_s4_floc('SAI00167635', Floc).



demo06(Floc) :- 
    aib_process_to_s4_floc('SAI00338989', Floc).

demo06a(Floc) :- 
    aib_process_to_s4_floc('SAI00075544', Floc).


demo07(X) :-
    %% TODO this probably wants to be wrapped in setof at the call site
    aib_floc_below('SAI00338990', X).



demo08(Xs) :- 
    %% TODO this probably wants to be wrapped in setof at the call site
    setof(X, aib_floc_to_s4_system('SAI00215602', X), Xs).


demo09(Xs) :- 
    %% TODO this probably wants to be wrapped in setof at the call site
    setof(X, aib_equipment_to_s4_system('PLI00358197', X), Xs).



demo10(X) :- 
    % installation
    aib_to_s4_floc('SAI00003608', X).

demo10a(X) :- 
    % process_group
    aib_to_s4_floc('SAI00167636', X).

demo10b(X) :- 
    % process
    % currently failing due to lack of data in s4 sample.
    aib_to_s4_floc('SAI00338989', X).

demo10c(X) :- 
    % process no process group
    aib_to_s4_floc('SAI00075544', X).

demo10d(X) :- 
    % plant
    aib_to_s4_floc('SAI00215602', X).


demo10e(X) :- 
    % plant item
    aib_to_s4_floc('SAI00215603', X).

