% s4_structs.pl

:- module(s4_structs,
            [ is_s4_site/1
            , s4_siten_data/3
            , make_s4_site/3
            , s4_site_floc/2
            , s4_site_name/2
            ]).

:- use_module(library(record)).


:- record s4_site(floc:atom, name:atom).