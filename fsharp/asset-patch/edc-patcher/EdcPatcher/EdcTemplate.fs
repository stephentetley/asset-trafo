// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module EdcTemplate =
    
    open System

    open AssetPatch.TemplatePatcher.Template
    open AssetPatch.TemplateCatalogue.Base

    open EdcPatcher.InputData
    open EdcPatcher.OSGB36

    let optString (source : string) : string option = 
        match source with
        | null | "" -> None
        |_ -> Some source

    /// Note input string might have hh:mm:ss suffix. 
    /// So take first 10 characters.
    let getInstallDate (source : string) : DateTime = 
        match DateTime.TryParseExact( s = source.Substring(startIndex=0, length=10)
                                    , format = "dd/MM/yyyy"
                                    , provider = Globalization.CultureInfo.InvariantCulture
                                    , style = Globalization.DateTimeStyles.None) with
        | true, date -> date
        | false, _ -> 
            new DateTime(year=1970, month=1, day=1)

    let aib_reference_leaf_instance (parameters : WorkListRow) : Class = 
        aib_reference 
            [ s4_aib_reference () 
              ai2_aib_reference parameters.``AI2 Equipment SAI Number``
              ai2_aib_reference parameters.``AI2 Equipment PLI Code``
            ]

    let lstnut_leaf_instance (parameters : WorkListRow) : Class = 
        lstnut
            [ uniclass_code ()
              uniclass_desc ()
              optional <| lstn_transducer_model parameters.``Transducer Model``
              optional <| lstn_transducer_serial_no parameters.``Transducer Serial Number``
              applyOptional (lstn_relay_function 6) (RelayFunction.TryParse parameters.``Relay 6 Function``)
            ]

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

        let installDate = getInstallDate parameters.``Install Date``


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
                              aib_reference_leaf_instance parameters
                              lstnut_leaf_instance parameters
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
    

