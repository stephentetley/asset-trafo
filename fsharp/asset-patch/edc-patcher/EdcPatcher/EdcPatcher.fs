// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module EdcPatcher =
    
    open System.IO

    open AssetPatch.Base.CompilerMonad
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Template
    open AssetPatch.TemplatePatcher.Catalogue
    open AssetPatch.TemplatePatcher.PatchCompiler

    open EdcPatcher.OSGB36
    open EdcPatcher.InputData
    open EdcPatcher.EdcTemplate


    type EdcOptions = 
        { UserName : string 
          OutputDirectory : string
          WorkListPath : string 
          }

    let internal makeOutputDirectory (dirName : string) : unit = 
        if not <| Directory.Exists(dirName) then
            Directory.CreateDirectory(dirName) |> ignore
        else ()
    
    let runEdcPatcherPhase1 (opts : EdcOptions) = 
        runCompiler (defaultEnv opts.UserName) 
            (compile { 
                let! worklist = 
                    readWorkList opts.WorkListPath |>> List.map (fun row -> (FuncLocPath.Create row.``Root S4 FuncLoc``, row))
                
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)

                do! compileFunctionPatches opts.OutputDirectory
                                "edc_patch"
                                edcTemplate
                                worklist
            })


    let runEdcPatcherPhase2 (opts : EdcOptions) = 
        runCompiler (defaultEnv opts.UserName) 
            <| materializeEquiClassValuaPatches opts.OutputDirectory
