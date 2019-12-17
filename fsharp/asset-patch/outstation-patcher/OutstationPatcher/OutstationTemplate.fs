// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace OutstationPatcher

module OutstationTemplate =
    
    open System

    open AssetPatch.TemplatePatcher.Template
    open AssetPatch.TemplateCatalogue
    open AssetPatch.Lib.Common
    open AssetPatch.Lib.OSGB36

    open OutstationPatcher.InputData


    /// Note - this is very flaky as ExcelProvider seems to have difficulty 
    /// with Excel's type casting.
    let getInstallDate (source : string) : DateTime = 
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
            ]



    let osTemplate (parameters : WorkListRow) : Function = 

        
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
            <| control_and_automation 
                [ east_north_common 
                  aib_reference_common
                ]
                [ 
                  networks
                    [ east_north_common
                      aib_reference_common
                    ]
                    [   
                      telemetry
                        [ east_north_common 
                          aib_reference_common   
                        ]
                        [   
                          telemetry_system "SYS01" "Telemetry System"
                            [ east_north_common 
                              aib_reference_common
                              
                            ]
                            _no_assemblies_
                            [ 
                            ]
                        ]
                    ]
                ]
    

