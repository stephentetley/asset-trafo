% floc_codes.pl

% see __load_clipboard, must load ['rules/common.pl'].

% ['rules/floc_codes.pl'].

%% installation
aib_abs_floc(Sai, F1) :- 
    aib_floc_l1_l2_installation(Sai, F1, _).

%% installation + site_type (virtual)
aib_abs_floc(Sai, F1, F2) :- 
    aib_floc_l1_l2_installation(Sai, F1, F2).


%% this table decl might be unnecessary...
%% :- table aib_abs_floc/4.

%% process group
aib_abs_floc(Sai, F1, F2, F3) :- 
    aib_floc_l3_process_group(Sai, F3, _, _, S2),
    aib_floc_l1_l2_installation(S2, F1, F2).


%% process
aib_abs_floc(Sai, F1, F2, F3, F4) :- 
    aib_floc_l4_process(Sai, F4, _, _, S2),
    aib_abs_floc(S2, F1, F2, F3).


aib_abs_floc(Sai, F1, F2, F3, F4) :- 
    aib_floc_l4_process(Sai, F4, _, _, S2),
    is_aib_installation(S2), 
    F3 = '',
    aib_abs_floc(S2, F1, F2).

%% plant
aib_abs_floc(Sai, F1, F2, F3, F4, F5) :- 
    aib_floc_l5_plant(Sai, F5, _, _, S2),
    aib_abs_floc(S2, F1, F2, F3, F4).



%% plant_item (parent = plant)
aib_abs_floc(Sai, F1, F2, F3, F4, F5, F6) :- 
    aib_floc_l6_plant_item(Sai, F6, _, _, S2),
    is_aib_plant(S2),
    aib_abs_floc(S2, F1, F2, F3, F4, F5).


%% plant_item (parent = process)
aib_abs_floc(Sai, F1, F2, F3, F4, F5, F6) :- 
    aib_floc_l6_plant_item(Sai, F6, _, _, S2),
    is_aib_process(S2), 
    F5 = '',
    aib_abs_floc(S2, F1, F2, F3, F4).

%% plant_item - plant_item
%% Note 'L6_plant_item' contains l7 plant items.
aib_abs_floc(Sai, F1, F2, F3, F4, F5, F6, F7) :- 
    aib_floc_l6_plant_item(Sai, F7, _, _, S2),
    aib_abs_floc(S2, F1, F2, F3, F4, F5, F6).

%% Wrap as list

% no case for Length=1 floc

get_aib_abs_floc(Sai, Xs) :-
    aib_abs_floc(Sai, F1, F2), 
    Xs = [F1, F2].

get_aib_abs_floc(Sai, Xs) :-
    aib_abs_floc(Sai, F1, F2, F3), 
    Xs = [F1, F2, F3].

get_aib_abs_floc(Sai, Xs) :-
    aib_abs_floc(Sai, F1, F2, F3, F4), 
    Xs = [F1, F2, F3, F4].

get_aib_abs_floc(Sai, Xs) :-
    aib_abs_floc(Sai, F1, F2, F3, F4, F5), 
    Xs = [F1, F2, F3, F4, F5].

get_aib_abs_floc(Sai, Xs) :-
    aib_abs_floc(Sai, F1, F2, F3, F4, F5, F6), 
    Xs = [F1, F2, F3, F4, F5, F6].

get_aib_abs_floc(Sai, Xs) :-
    aib_abs_floc(Sai, F1, F2, F3, F4, F5, F6, F7), 
    Xs = [F1, F2, F3, F4, F5, F6, F7].

%% 

% aib_abs_floc('SAI00003608', F1).
% aib_abs_floc('SAI00167635', F1, F2, F3).
% aib_abs_floc('SAI00338989', F1, F2, F3, F4).

% No PG
% aib_abs_floc('SAI00075544', F1, F2, F3, F4).

% aib_abs_floc('SAI00075551', F1, F2, F3, F4, F5).

% No PG, but Plant above Plant Item...
% aib_abs_floc('AFL00329326', F1, F2, F3, F4, F5, F6).

% No Plant above Plant Item (parent = process)...
% aib_abs_floc('AFL00076753', F1, F2, F3, F4, F5, F6).

% Plant item above plant item
% aib_abs_floc('SAI00415646', F1, F2, F3, F4, F5, F6, F7).

% get_aib_abs_floc('SAI00003608', Xs).
% get_aib_abs_floc('AFL00076753', Xs).
% get_aib_abs_floc('SAI00415646', Xs).