// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module EdcPatcher =
    
    open System.IO

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.PatchCompiler

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
                    readWorkList opts.WorkListPath |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)

                do! compileFunctionPatches opts.OutputDirectory
                                "edc_patch"
                                edcTemplate
                                worklist
            })


    let runEdcPatcherPhase2 (opts : EdcOptions) = 
        runCompiler (defaultEnv opts.UserName) 
            <| compile {
                let subfolders = Directory.GetDirectories opts.OutputDirectory |> Array.toList
                do! mapMz materializeEquiClassValuaPatches subfolders
                return ()
            }
            
