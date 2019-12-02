// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module EdcPatcher =
    
    open AssetPatch.TemplatePatcher.Template
    open AssetPatch.TemplatePatcher.Catalogue

    open EdcPatcher.OSGB36
    open EdcPatcher.InputData
    open EdcPatcher.EdcTemplate

    
    //let runEdcPatcher () = 
    //    let worklist = 
    //        [ ("KRI03", {Code = "KRI03"; Name = "Kriddle SPS"; Easting = 492729; Northing = 477323; EquiFlocSaiNumber = Some "SAI00043252"; EquiPliNumber = Some "PLI00002001" } )
    //        ] 
    //        |> List.map (fun (name, v) -> (FuncLocPath.Create name, v))
    //    runCompiler (defaultEnv "TETLEYS") 
    //       <| compileFunctionPatches 
    //                   (outputDirectory "edc-patches")
    //                   "env_discharge"
    //                   edcTemplate
    //                   worklist
