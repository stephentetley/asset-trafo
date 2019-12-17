﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module EdcPatcher =
    
    open System.IO

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.PatchCompiler

    open EdcPatcher.InputData
    open EdcPatcher.EdcTemplate


    type EdcPatcherOptions = 
        { UserName : string 
          OutputDirectory : string
          WorkListPath : string
          }

    let private makeCompilerOptions (opts : EdcPatcherOptions) : CompilerOptions = 
        { UserName = opts.UserName }

    let internal makeOutputDirectory (dirName : string) : unit = 
        if not <| Directory.Exists(dirName) then
            Directory.CreateDirectory(dirName) |> ignore
        else ()
    
    let runEdcPatcherPhase1 (opts : EdcPatcherOptions) : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts           
        runCompiler compilerOpts None
            <| compile { 
                let! worklist = 
                    readWorkList opts.WorkListPath |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)

                do! compileFunctionPatchesPhase1 opts.OutputDirectory
                                "edc_patch"
                                edcTemplate
                                worklist
            }

    /// Phase 2 generates ClassEqui and ValuaEqui patches 
    /// with materialized Equipment numbers
    let runEdcPatcherPhase2 (opts : EdcPatcherOptions) 
                            (equipmentDownloadPath : string)  : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts  
        runCompiler compilerOpts (Some equipmentDownloadPath)
            <| compile { 
                let! worklist = 
                    readWorkList opts.WorkListPath |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
            
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)

                do! compileFunctionPatchesPhase2 opts.OutputDirectory
                                "edc_patch"
                                edcTemplate
                                worklist
            }

    