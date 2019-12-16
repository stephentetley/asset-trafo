// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module Emitter =

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.EmitEquipment
    open AssetPatch.TemplatePatcher.EmitFuncLoc

    type Phase1Data = 
        { FlocData : Phase1FlocData
          EquiData : Phase1EquiData
        }

    type Phase2Data =  Phase1FlocData

    let private collectPhase1Data (xs : (FuncLocResult1 * Phase1EquiData) list) : Phase1Data = 
        let flocResults = xs|> List.map fst |> concatFuncLocResult1s
        let equiResults = xs|> List.map snd |> concatPhase1EquiData
        { FlocData = flocResults; EquiData = equiResults }

    let private concatPhase1Data (xs : Phase1Data list) : Phase1Data = 
        { FlocData = xs |> List.map (fun x -> x.FlocData) |> concatPhase1FlocData
          EquiData = xs |> List.map (fun x -> x.EquiData) |> concatPhase1EquiData
        }
        

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

