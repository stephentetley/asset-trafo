// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module Emitter =

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.EmitCommon
    open AssetPatch.TemplatePatcher.EmitEquipment
    open AssetPatch.TemplatePatcher.EmitFuncLoc


    // ************************************************************************
    // Phase 1


    
        

    let private componentEmitPhase1 (source : S4Component) : CompilerMonad<FuncLocResult1 * Phase1EquiData> = 
        compile {
            let! flocResult = 
                funclocToFuncLocResult1 source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = 
                equipmentsToPhase1EquiData source.FuncLoc source.FlocProperties source.Equipment
            return (flocResult, equiResults)
        }

    let componentsEmitPhase1 (source : S4Component list) : CompilerMonad<Phase1Data> = 
        mapM componentEmitPhase1 source |>> collectPhase1Data


    let private itemEmitPhase1 (source : S4Item) : CompilerMonad<FuncLocResult1 * Phase1EquiData> = 
        compile {
            let! flocResult = 
                funclocToFuncLocResult1 source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = 
                equipmentsToPhase1EquiData source.FuncLoc source.FlocProperties source.Equipment
            return (flocResult, equiResults)
        }

    let itemsEmitPhase1 (source : S4Item list) : CompilerMonad<Phase1Data> = 
        compile { 
            let! ans1 = mapM itemEmitPhase1 source |>> collectPhase1Data
            let! kids = mapM (fun (x : S4Item) -> componentsEmitPhase1 x.Components) source
            return concatPhase1Data (ans1 :: kids)
        }



    let private assemblyEmitPhase1 (source : S4Assembly) : CompilerMonad<FuncLocResult1 * Phase1EquiData> = 
        compile {
            let! flocResult = 
                funclocToFuncLocResult1 source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = 
                equipmentsToPhase1EquiData source.FuncLoc source.FlocProperties source.Equipment
            return (flocResult, equiResults)
        }

    let assembliesEmitPhase1 (source : S4Assembly list) : CompilerMonad<Phase1Data> = 
        compile { 
            let! ans1 = mapM assemblyEmitPhase1 source |>> collectPhase1Data
            let! kids = mapM (fun (x : S4Assembly) -> itemsEmitPhase1 x.Items) source
            return concatPhase1Data (ans1 :: kids)
        }


    let private systemEmitPhase1 (source : S4System) : CompilerMonad<FuncLocResult1 * Phase1EquiData> = 
        compile {
            let! flocResult = 
                funclocToFuncLocResult1 source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = 
                equipmentsToPhase1EquiData source.FuncLoc source.FlocProperties source.Equipment
            return (flocResult, equiResults)
        }

    let systemsEmitPhase1 (source : S4System list) : CompilerMonad<Phase1Data> = 
        compile { 
            let! ans1 = mapM systemEmitPhase1 source |>> collectPhase1Data
            let! kids = mapM (fun (x : S4System) -> assembliesEmitPhase1 x.Assemblies) source
            return concatPhase1Data (ans1 :: kids)
        }

    let private processEmitPhase1 (source : S4Process) : CompilerMonad<FuncLocResult1> = 
        funclocToFuncLocResult1 source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
        

    let processesEmitPhase1 (source : S4Process list) : CompilerMonad<Phase1Data> = 
        compile { 
            let! xs = mapM processEmitPhase1 source 
            let ans1 = { FlocData = concatFuncLocResult1s xs ; EquiData = emptyPhase1EquiData }
            let! kids = mapM (fun (x : S4Process) -> systemsEmitPhase1 x.Systems) source
            return concatPhase1Data (ans1 :: kids)
        }

    let private processGroupEmitPhase1 (source : S4ProcessGroup) : CompilerMonad<FuncLocResult1> = 
        funclocToFuncLocResult1 source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
        

    let processGroupsEmitPhase1 (source : S4ProcessGroup list) : CompilerMonad<Phase1Data> = 
        compile { 
            let! xs = mapM processGroupEmitPhase1 source 
            let ans1 = { FlocData = concatFuncLocResult1s xs ; EquiData = emptyPhase1EquiData }
            let! kids = mapM (fun (x : S4ProcessGroup) -> processesEmitPhase1 x.Processes) source
            return concatPhase1Data (ans1 :: kids)
        }


    let private functionEmitPhase1 (source : S4Function) : CompilerMonad<FuncLocResult1> = 
        funclocToFuncLocResult1 source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
        

    let functionsEmitPhase1 (source : S4Function list) : CompilerMonad<Phase1Data> = 
        compile { 
            let! xs = mapM functionEmitPhase1 source 
            let ans1 = { FlocData = concatFuncLocResult1s xs ; EquiData = emptyPhase1EquiData }
            let! kids = mapM (fun (x : S4Function) -> processGroupsEmitPhase1 x.ProcessGroups) source
            return concatPhase1Data (ans1 :: kids)
        }

        
    let siteEmitPhase1 (source : S4Site) : CompilerMonad<FuncLocResult1> = 
        funclocToFuncLocResult1 source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            

    let sitesEmitPhase1 (source : S4Site list) : CompilerMonad<Phase1Data> = 
        compile { 
            let! xs = mapM siteEmitPhase1 source 
            let ans1 = { FlocData = concatFuncLocResult1s xs ; EquiData = emptyPhase1EquiData }
            let! kids = mapM (fun (x : S4Site) -> functionsEmitPhase1 x.Functions) source
            return concatPhase1Data (ans1 :: kids)
        }

    // ************************************************************************
    // Phase 2

    type Phase2Data =  Phase2EquiData

    let private componentEmitPhase2 (source : S4Component) : CompilerMonad<Phase2Data> = 
        equipmentsToPhase2EquiData source.FuncLoc source.FlocProperties source.Equipment

    let componentsEmitPhase2 (source : S4Component list) : CompilerMonad<Phase2Data> = 
        mapM componentEmitPhase2 source |>> concatPhase2EquiData

    let private itemEmitPhase2 (source : S4Item) : CompilerMonad<Phase2Data> = 
        equipmentsToPhase2EquiData source.FuncLoc source.FlocProperties source.Equipment


    let itemsEmitPhase2 (source : S4Item list) : CompilerMonad<Phase2Data> = 
        compile { 
            let! ans1 = mapM itemEmitPhase2 source |>> concatPhase2EquiData
            let! kids = mapM (fun (x : S4Item) -> componentsEmitPhase2 x.Components) source
            return concatPhase2EquiData (ans1 :: kids)
        }

    let private assemblyEmitPhase2 (source : S4Assembly) : CompilerMonad<Phase2Data> = 
        equipmentsToPhase2EquiData source.FuncLoc source.FlocProperties source.Equipment


    let assembliesEmitPhase2 (source : S4Assembly list) : CompilerMonad<Phase2Data> = 
        compile { 
            let! ans1 = mapM assemblyEmitPhase2 source |>> concatPhase2EquiData
            let! kids = mapM (fun (x : S4Assembly) -> itemsEmitPhase2 x.Items) source
            return concatPhase2EquiData (ans1 :: kids)
        }

    let private systemEmitPhase2 (source : S4System) : CompilerMonad<Phase2Data> = 
        equipmentsToPhase2EquiData source.FuncLoc source.FlocProperties source.Equipment


    let systemsEmitPhase2 (source : S4System list) : CompilerMonad<Phase2Data> = 
        compile { 
            let! ans1 = mapM systemEmitPhase2 source |>> concatPhase2EquiData
            let! kids = mapM (fun (x : S4System) -> assembliesEmitPhase2 x.Assemblies) source
            return concatPhase2EquiData (ans1 :: kids)
        }



    let processesEmitPhase2 (source : S4Process list) : CompilerMonad<Phase2Data> = 
        compile { 
            let! kids = mapM (fun (x : S4Process) -> systemsEmitPhase2 x.Systems) source
            return concatPhase2EquiData kids
        }

    let processGroupsEmitPhase2 (source : S4ProcessGroup list) : CompilerMonad<Phase2Data> = 
        compile { 
            let! kids = mapM (fun (x : S4ProcessGroup) -> processesEmitPhase2 x.Processes) source
            return concatPhase2EquiData kids
        }

    let functionsEmitPhase2 (source : S4Function list) : CompilerMonad<Phase2Data> = 
        compile { 
            let! kids = mapM (fun (x : S4Function) -> processGroupsEmitPhase2 x.ProcessGroups) source
            return concatPhase2EquiData kids
        }

    let sitesEmitPhase2 (source : S4Site list) : CompilerMonad<Phase2Data> = 
        compile { 
            let! kids = mapM (fun (x : S4Site) -> functionsEmitPhase2 x.Functions) source
            return concatPhase2EquiData kids
        }
