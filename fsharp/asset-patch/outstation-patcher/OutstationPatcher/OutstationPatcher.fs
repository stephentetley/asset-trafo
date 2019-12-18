// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace OutstationPatcher

module OutstationPatcher =
    
    open System.IO

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.EmitCommon
    open AssetPatch.TemplatePatcher.Emitter
    open AssetPatch.TemplatePatcher.PatchCompiler

    open OutstationPatcher.InputData
    open OutstationPatcher.OutstationTemplate


    type OsPatcherOptions = 
        { UserName : string 
          OutputDirectory : string
          WorkListPath : string
        }

    let private makeCompilerOptions (opts : OsPatcherOptions) : CompilerOptions = 
        { UserName = opts.UserName }

    let internal makeOutputDirectory (dirName : string) : unit = 
        if not <| Directory.Exists(dirName) then
            Directory.CreateDirectory(dirName) |> ignore
        else ()
    
    /// Note - we need to be able to create floc patches at different
    /// levels in the tree (according to what already exists).
    /// This will need changes to TemplatePatcher...

    let private phase1ProcessRow (path : FuncLocPath, row : WorkListRow) : CompilerMonad<Phase1Data> = 
        match path.Level with
        | 1 -> applyFlocTemplate1 (path, row) osLevel2Template >>= function1EmitPhase1
        | 2 -> applyFlocTemplate1 (path, row) osLevel3Template >>= processGroup1EmitPhase1
        | 3 -> applyFlocTemplate1 (path, row) osLevel4Template >>= process1EmitPhase1
        | 4 -> applyFlocTemplate1 (path, row) osLevel5Template >>= system1EmitPhase1
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)

    let runOutstationPatcherPhase1 (opts : OsPatcherOptions) : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts           
        runCompiler compilerOpts None
            <| compile { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
                let! worklist = 
                    readWorkList opts.WorkListPath |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                let! phase1Data = mapM phase1ProcessRow worklist |>> Phase1Data.Concat
                do! writePhase1Data opts.OutputDirectory "outstation_patch" phase1Data
                return ()
            }

    let private phase2ProcessRow (path : FuncLocPath, row : WorkListRow) : CompilerMonad<Phase2Data> = 
        match path.Level with
        | 1 -> applyFlocTemplate1 (path, row) osLevel2Template >>= function1EmitPhase2
        | 2 -> applyFlocTemplate1 (path, row) osLevel3Template >>= processGroup1EmitPhase2
        | 3 -> applyFlocTemplate1 (path, row) osLevel4Template >>= process1EmitPhase2
        | 4 -> applyFlocTemplate1 (path, row) osLevel5Template >>= system1EmitPhase2
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)


    /// Phase 2 materializes ClassEqui and ValuaEqui patches
    let runOutstationPatcherPhase2 (opts : OsPatcherOptions) 
                            (equipmentDownloadPath : string)  : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts  
        runCompiler compilerOpts (Some equipmentDownloadPath)
            <| compile { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
                let! worklist = 
                    readWorkList opts.WorkListPath |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                let! phase2Data = mapM phase2ProcessRow worklist |>> Phase2Data.Concat
                do! writePhase2Data opts.OutputDirectory "outstation_patch" phase2Data
                return ()
            }

    