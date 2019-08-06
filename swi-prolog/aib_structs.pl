% aib_structs.pl

:- module(aib_structs,
            [ is_aib_installation/1
            , aib_installation_data/3
            , make_aib_installation/3
            , aib_installation_ref/2
            , aib_installation_name/2
            , aib_installation_type/2
            
            , is_aib_process_group/1
            , aib_process_group_data/3
            , make_aib_process_group/3
            , aib_process_group_ref/2
            , aib_process_group_name/2
            , aib_process_group_type/2
            , aib_process_group_parent/2
            
            , is_aib_process/1
            , aib_process_data/3
            , make_aib_process/3
            , aib_process_ref/2
            , aib_process_name/2
            , aib_process_type/2
            , aib_process_parent/2
            
            ]).

:- use_module(library(record)).


:- record aib_installation(ref:atom, name:atom, type:atom).

:- record aib_process_group(ref:atom, name:atom, type:atom, parent:atom).

:- record aib_process(ref:atom, name:atom, type:atom, parent:atom).