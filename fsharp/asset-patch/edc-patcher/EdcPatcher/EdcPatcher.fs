// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module EdcPatcher =
    
    open System.IO

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.Emitter
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
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                let! xs = 
                    readWorkList opts.WorkListPath |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))             
                let! worklist1 = applyFlocTemplate xs edcTemplate
                let! phase1Data = functionListEmitPhase1 worklist1
                do! writePhase1Data opts.OutputDirectory "edc_patch" phase1Data
                return ()
            }

    /// Phase 2 generates ClassEqui and ValuaEqui patches 
    /// with materialized Equipment numbers
    let runEdcPatcherPhase2 (opts : EdcPatcherOptions) 
                            (equipmentDownloadPath : string) : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts  
        runCompiler compilerOpts (Some equipmentDownloadPath)
            <| compile { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                let! xs = 
                    readWorkList opts.WorkListPath |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))             
                let! worklist1 = applyFlocTemplate xs edcTemplate
                let! phase2Data = functionListEmitPhase2 worklist1
                do! writePhase2Data opts.OutputDirectory "edc_patch" phase2Data
                return ()
            }

    