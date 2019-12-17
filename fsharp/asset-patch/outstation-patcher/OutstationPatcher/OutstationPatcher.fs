// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace OutstationPatcher

module OutstationPatcher =
    
    open System.IO

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
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

    let runOutstationPatcherPhase1 (opts : OsPatcherOptions) : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts           
        runCompiler compilerOpts None
            <| compile { 
                let! worklist = 
                    readWorkList opts.WorkListPath |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)

                do! compileFunctionPatchesPhase1 opts.OutputDirectory
                                "outstation_patch"
                                osTemplate
                                worklist
            }

    /// Phase 2 materializes Floc patches
    let runOutstationPatcherPhase2 (opts : OsPatcherOptions) 
                            (equipmentDownloadPath : string)  : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts  
        runCompiler compilerOpts (Some equipmentDownloadPath)
            <| compile { 
                let! worklist = 
                    readWorkList opts.WorkListPath |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
            
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)

                do! compileFunctionPatchesPhase2 opts.OutputDirectory
                                "outstation_patch"
                                osTemplate
                                worklist
            }

    