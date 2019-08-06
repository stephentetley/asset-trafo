% s4_structs.pl

:- module(s4_structs,
            [ is_s4_site/1
            , s4_site_data/3
            , make_s4_site/3
            , s4_site_floc/2
            , s4_site_name/2

            , is_s4_function/1
            , s4_function_data/3
            , make_s4_function/3
            , s4_function_floc/2
            , s4_function_floc_suffix/2
            , s4_function_name/2
            , s4_function_parent_floc/2

            , is_s4_process_group/1
            , s4_process_group_data/3
            , make_s4_process_group/3
            , s4_process_group_floc/2
            , s4_process_group_floc_suffix/2
            , s4_process_group_name/2
            , s4_process_group_parent_floc/2

            , is_s4_process/1
            , s4_process_data/3
            , make_s4_process/3
            , s4_process_floc/2
            , s4_process_floc_suffix/2
            , s4_process_name/2
            , s4_process_parent_floc/2

            , is_s4_system/1
            , s4_system_data/3
            , make_s4_system/3
            , s4_system_floc/2
            , s4_system_floc_suffix/2
            , s4_system_name/2
            , s4_system_parent_floc/2

            , is_s4_assembly/1
            , s4_assembly_data/3
            , make_s4_assembly/3
            , s4_assembly_floc/2
            , s4_assembly_floc_suffix/2
            , s4_assembly_name/2
            , s4_assembly_aib_ref/2
            , s4_assembly_floc_pli/2
            , s4_assembly_equipment_pli/2
            , s4_assembly_parent_floc/2

            , is_s4_item/1
            , s4_item_data/3
            , make_s4_item/3
            , s4_item_floc/2
            , s4_item_floc_suffix/2
            , s4_item_name/2
            , s4_item_aib_ref/2
            , s4_item_floc_pli/2
            , s4_item_parent_pli/2
            , s4_item_equipment_pli/2
            , s4_item_parent_floc/2

            ]).

:- use_module(library(record)).


% Level 1
:- record s4_site(floc:atom, name:atom).

% Level 2
:- record s4_function(floc:atom, floc_suffix:atom, name:atom, parent_floc:atom).

% Level 3
:- record s4_process_group(floc:atom, floc_suffix:atom, name:atom, parent_floc:atom).

% Level 4
:- record s4_process(floc:atom, floc_suffix:atom, name:atom, parent_floc:atom).

% Level 5
:- record s4_system(floc:atom, floc_suffix:atom, name:atom, parent_floc:atom).

% Level 6
:- record s4_assembly(floc:atom, floc_suffix:atom, name:atom, aib_ref:atom, 
                        floc_pli:atom, equipment_pli:atom, parent_floc:atom).

% Level 7
:- record s4_item(floc:atom, floc_suffix:atom, name:atom, aib_ref:atom, 
                    floc_pli:atom, parent_pli:atom, equipment_pli:atom, parent_floc:atom).                    