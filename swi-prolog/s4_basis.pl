% s4_basis.pl


:- module(s4_basis,
    [ is_s4_site/1
    , is_s4_function/1
    , is_s4_process_group/1
    , is_s4_process/1
    , is_s4_system/1
    , is_s4_assembly/1
    , is_s4_item/1
    , is_s4_component/1
    , is_s4_equipment/1

    , atoms_to_floc/2
    , floc_take/3
    , floc_prefix/2

    ]).

:- use_module(library(db_facts)).

%%% 

is_s4_site(Floc) :- 
    db_holds(assets, s4_site([s4_floc=Floc, name=_])), 
    !.

is_s4_function(Floc) :- 
    db_holds(assets, s4_function([s4_floc=Floc, name=_])), 
    !.

is_s4_process_group(Floc) :- 
    db_holds(assets, s4_process_group([s4_floc=Floc, name=_])), 
    !.


is_s4_process(Floc) :- 
    db_holds(assets, s4_process([s4_floc=Floc, name=_])), 
    !.

is_s4_system(Floc) :- 
    db_holds(assets, s4_system([s4_floc=Floc, name=_])), 
    !.

is_s4_assembly(Floc) :- 
    db_holds(assets, s4_assembly([s4_floc=Floc, name=_])), 
    !.

is_s4_item(Floc) :- 
    db_holds(assets, s4_item([s4_floc=Floc, name=_])), 
    !.

is_s4_component(Floc) :- 
    db_holds(assets, s4_component([s4_floc=Floc, name=_])), 
    !.

%% Ref is a BIGINT, printed with leading zeros in the initial extraction
is_s4_equipment(Ref) :- 
    db_holds(assets, s4_component([s4_ref=Ref, name=_])), 
    !.


%%% Manipulate s4 FLOC symbols

atoms_to_floc(List, Floc) :- 
    atomics_to_string(List, "-", S1),
    string_to_atom(S1, Floc).
    

% if count > len take whole list.
list_take(Src, Count, List) :- 
    findall(Elt, (nth1(Ix, Src, Elt), Ix =< Count), List).

floc_take(Src, Count, Floc) :- 
    atomics_to_string(List, "-", Src),
    list_take(List, Count, List1),
    atoms_to_floc(List1, Floc).


floc_prefix(Floc, Prefix) :- 
    sub_atom(Floc, 0, _, _, Prefix).