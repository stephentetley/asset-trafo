// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module EdcTemplate =
    
    open AssetPatch.TemplatePatcher.Template
    open AssetPatch.TemplatePatcher.Catalogue

    open EdcPatcher.InputData
    open EdcPatcher.OSGB36

    let edcTemplate (parameters : WorkListRow) : Function = 
        let east_north_common = 
            match NGR.Create parameters.NGR |> Option.map ngrToEastingNorthing with
            | Some eastNorth -> 
                east_north [ easting eastNorth.Easting; northing eastNorth.Northing ]
            | None -> 
                east_north [ easting 0; northing 0 ]
        environmental_discharge 
            [ east_north_common 
              aib_reference [ s4_aib_reference () ] 
            ]
            [ 
              liquid_discharge
                [ east_north_common
                  aib_reference [ s4_aib_reference () ] 
                ]
                [   
                  regulatory_monitoring
                    [ east_north_common 
                      aib_reference [ s4_aib_reference () ]    
                    ]
                    [   
                      montoring_system "SYS01" "EA Event Duration Monitoring"
                        [ east_north_common 
                          aib_reference [ s4_aib_reference () ]
                        ]
                        _no_assemblies_
                        [ 
                          lstn_level_transmitter "Storm Overflow Level Monitor Loop"
                            [ east_north_common
                              aib_reference [ s4_aib_reference () ]
                            ]
                            _no_subordinate_equipment_
                            [ manufacturer parameters.Manufacturer
                              model parameters.Model
                              serial_number parameters.``Serial Number``
                            ]
                        ]
                    ]
                ]
            ]
    

