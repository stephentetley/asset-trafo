// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module EdcTemplate =
    
    open System

    open AssetPatch.TemplatePatcher.Template
    open AssetPatch.TemplateCatalogue
    open AssetPatch.TemplateCatalogue.Smonsy
    open AssetPatch.Lib.Common
    open AssetPatch.Lib.OSGB36

    open EdcPatcher.InputData

    let optString (source : string) : string option = 
        match source with
        | null | "" -> None
        |_ -> Some source

    /// Note - this is very flaky as ExcelProvider seems to have difficulty 
    /// with Excel's type casting.
    let getInstallDate (source : string) : DateTime = 
        match source with
        | null | "" -> new DateTime(year=1970, month=1, day=1)
        | _ -> 
            match tryGetUSDate source with
            | Some date -> date
            | None -> new DateTime(year=1970, month=1, day=1)


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
              optional <| lstn_transducer_model     parameters.``Transducer Model``
              optional <| lstn_transducer_serial_no parameters.``Transducer Serial Number``
              applyOptional (lstn_relay_function 1) (RelayFunction.TryParse parameters.``Relay 1 Function``)
              applyOptional (lstn_relay_on_level 1) (tryGetDecimal parameters.``Relay 1 On``)
              applyOptional (lstn_relay_off_level 1) (tryGetDecimal parameters.``Relay 1 Off``)
              
              applyOptional (lstn_relay_function 2) (RelayFunction.TryParse parameters.``Relay 2 Function``)
              applyOptional (lstn_relay_on_level 2) (tryGetDecimal parameters.``Relay 2 On``)
              applyOptional (lstn_relay_off_level 2) (tryGetDecimal parameters.``Relay 2 Off``)
              
              applyOptional (lstn_relay_function 3) (RelayFunction.TryParse parameters.``Relay 3 Function``)
              applyOptional (lstn_relay_on_level 3) (tryGetDecimal parameters.``Relay 3 On``)
              applyOptional (lstn_relay_off_level 3) (tryGetDecimal parameters.``Relay 3 Off``)
              
              applyOptional (lstn_relay_function 4) (RelayFunction.TryParse parameters.``Relay 4 Function``)
              applyOptional (lstn_relay_on_level 4) (tryGetDecimal parameters.``Relay 4 On``)
              applyOptional (lstn_relay_off_level 4) (tryGetDecimal parameters.``Relay 4 Off``)
              
              applyOptional (lstn_relay_function 5) (RelayFunction.TryParse parameters.``Relay 5 Function``)
              applyOptional (lstn_relay_on_level 5) (tryGetDecimal parameters.``Relay 5 On``)
              applyOptional (lstn_relay_off_level 5) (tryGetDecimal parameters.``Relay 5 Off``)

              applyOptional (lstn_relay_function 6) (RelayFunction.TryParse parameters.``Relay 6 Function``)
              applyOptional (lstn_relay_on_level 6) (tryGetDecimal parameters.``Relay 6 On``)
              applyOptional (lstn_relay_off_level 6) (tryGetDecimal parameters.``Relay 6 Off``)
              
            ]



    let edcTemplate (parameters : WorkListRow) : Function = 

        
        let installDate = getInstallDate parameters.``Install Date``

        let startupDateTrafo : EnvTransformer = 
            match tryGetUSDate parameters.``Install Date`` with
            | None -> id
            | Some date -> startupDate date

        let east_north_common = 
            match NGR.Create parameters.NGR with
            | Some eastNorth -> east_north_ngr eastNorth
            | None ->  east_north [ easting 0; northing 0 ]

        let aib_reference_common = 
            aib_reference 
                [   s4_aib_reference ()
                    ai2_aib_reference parameters.``AI2 Site Reference``
                    
                ]


        locals [startupDateTrafo]
            <| environmental_discharge 
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
                              smonsy 
                                [ system_type "EA Overflow Monitoring" 
                                ]
                            ]
                            _no_assemblies_
                            [ 
                              lstn_level_transmitter "Storm Overflow Level Monitor Loop"
                                [ east_north_common
                                  aib_reference_leaf_instance parameters
                                  lstnut_leaf_instance parameters
                                  asset_condition_new_item (uint32 installDate.Year)
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
    

