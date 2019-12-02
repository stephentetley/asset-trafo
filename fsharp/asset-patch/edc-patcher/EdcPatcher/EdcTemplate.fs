// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module EdcTemplate =
    
    open System

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

        let aib_reference_common = 
            aib_reference 
                [   s4_aib_reference ()
                    ai2_aib_reference parameters.``AI2 Site Reference``
                ]

        let installDate = 
            printfn "Install Date: %s" parameters.``Install Date``
            match DateTime.TryParseExact( s = parameters.``Install Date``
                                        , format="dd/MM/yyyy"
                                        , provider = Globalization.CultureInfo.InvariantCulture
                                        , style = Globalization.DateTimeStyles.None) with
            | true, date -> date
            | false, _ -> new DateTime(year=1970, month=1, day=1)

        environmental_discharge 
            [ east_north_common 
              aib_reference_common
            ]
            [ 
              liquid_discharge
                [ east_north_common
                  aib_reference_common
                ]
                [   
                  regulatory_monitoring
                    [ east_north_common 
                      aib_reference_common   
                    ]
                    [   
                      montoring_system "SYS01" "EA Event Duration Monitoring"
                        [ east_north_common 
                          aib_reference_common
                        ]
                        _no_assemblies_
                        [ 
                          lstn_level_transmitter "Storm Overflow Level Monitor Loop"
                            [ east_north_common
                              aib_reference 
                                [ s4_aib_reference () 
                                  ai2_aib_reference parameters.``AI2 Equipment SAI Number``
                                  ai2_aib_reference parameters.``AI2 Equipment PLI Code``
                                ]
                            ]
                            _no_subordinate_equipment_
                            [ manufacturer parameters.Manufacturer
                              model parameters.Model
                              serial_number parameters.``Serial Number``
                              construction_year (uint16 installDate.Year)
                              construction_month (uint8 installDate.Month)
                            ]
                        ]
                    ]
                ]
            ]
    

