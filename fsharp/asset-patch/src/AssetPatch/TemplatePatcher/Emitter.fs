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


    let component1EmitPhase1 (source : S4Component) : CompilerMonad<Phase1Data> = 
        compile {
            let! flocResult = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = 
                equipmentsToPhase1EquiData source.FuncLoc source.FlocProperties source.Equipment
            return { FlocData = flocResult; EquiData = equiResults } 
        }

    let componentListEmitPhase1 (source : S4Component list) : CompilerMonad<Phase1Data> = 
        mapM component1EmitPhase1 source |>> Phase1Data.Concat

    
    let item1EmitPhase1 (source : S4Item) : CompilerMonad<Phase1Data> = 
        compile {
            let! flocResult = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = 
                equipmentsToPhase1EquiData source.FuncLoc source.FlocProperties source.Equipment
            let  x1 = { FlocData = flocResult; EquiData = equiResults } 
            let! x2 = componentListEmitPhase1 source.Components
            return  Phase1Data.Concat [x1; x2]
        }

    let itemListEmitPhase1 (source : S4Item list) : CompilerMonad<Phase1Data> = 
        mapM item1EmitPhase1 source |>> Phase1Data.Concat
        



    let assembly1EmitPhase1 (source : S4Assembly) : CompilerMonad<Phase1Data> = 
        compile {
            let! flocResult = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = 
                equipmentsToPhase1EquiData source.FuncLoc source.FlocProperties source.Equipment
            let  x1 = { FlocData = flocResult; EquiData = equiResults } 
            let! x2 = itemListEmitPhase1 source.Items
            return  Phase1Data.Concat [x1; x2]
        }

    let assemblyListEmitPhase1 (source : S4Assembly list) : CompilerMonad<Phase1Data> = 
        mapM assembly1EmitPhase1 source |>> Phase1Data.Concat
        

    let system1EmitPhase1 (source : S4System) : CompilerMonad<Phase1Data> = 
        compile {
            let! flocResult = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = 
                equipmentsToPhase1EquiData source.FuncLoc source.FlocProperties source.Equipment
            let  x1 = { FlocData = flocResult; EquiData = equiResults } 
            let! x2 = assemblyListEmitPhase1 source.Assemblies
            return  Phase1Data.Concat [x1; x2]
        }

    let systemListEmitPhase1 (source : S4System list) : CompilerMonad<Phase1Data> = 
        mapM system1EmitPhase1 source |>> Phase1Data.Concat
        

    let process1EmitPhase1 (source : S4Process) : CompilerMonad<Phase1Data> = 
        compile {
            let! flocResult = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let  x1 = { FlocData = flocResult; EquiData = Phase1EquiData.Empty } 
            let! x2 = systemListEmitPhase1 source.Systems
            return  Phase1Data.Concat [x1; x2]
        }

    let processListEmitPhase1 (source : S4Process list) : CompilerMonad<Phase1Data> = 
        mapM process1EmitPhase1 source |>> Phase1Data.Concat


    let processGroup1EmitPhase1 (source : S4ProcessGroup) : CompilerMonad<Phase1Data> = 
        compile {
            let! flocResult = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let  x1 = { FlocData = flocResult; EquiData = Phase1EquiData.Empty } 
            let! x2 = processListEmitPhase1 source.Processes
            return  Phase1Data.Concat [x1; x2]
        }

    let processGroupListEmitPhase1 (source : S4ProcessGroup list) : CompilerMonad<Phase1Data> = 
        mapM processGroup1EmitPhase1 source |>> Phase1Data.Concat


    let functionEmitPhase1 (source : S4Function) : CompilerMonad<Phase1Data> = 
        compile {
            let! flocResult = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let  x1 = { FlocData = flocResult; EquiData = Phase1EquiData.Empty } 
            let! x2 = processGroupListEmitPhase1 source.ProcessGroups
            return  Phase1Data.Concat [x1; x2]
        }

    let functionListEmitPhase1 (source : S4Function list) : CompilerMonad<Phase1Data> = 
        mapM functionEmitPhase1 source |>> Phase1Data.Concat

        
    let site1EmitPhase1 (source : S4Site) : CompilerMonad<Phase1Data> = 
        compile {
            let! flocResult = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let  x1 = { FlocData = flocResult; EquiData = Phase1EquiData.Empty } 
            let! x2 = functionListEmitPhase1 source.Functions
            return Phase1Data.Concat [x1; x2]
        }

    let siteListEmitPhase1 (source : S4Site list) : CompilerMonad<Phase1Data> = 
        mapM site1EmitPhase1 source |>> Phase1Data.Concat

    // ************************************************************************
    // Phase 2

    type Phase2Data =  Phase2EquiData

    let component1EmitPhase2 (source : S4Component) : CompilerMonad<Phase2Data> = 
        equipmentsToPhase2EquiData source.FuncLoc source.FlocProperties source.Equipment

    let componentListEmitPhase2 (source : S4Component list) : CompilerMonad<Phase2Data> = 
        mapM component1EmitPhase2 source |>> Phase2EquiData.Concat

    let item1EmitPhase2 (source : S4Item) : CompilerMonad<Phase2Data> = 
        compile { 
            let! x1 = equipmentsToPhase2EquiData source.FuncLoc source.FlocProperties source.Equipment
            let! x2 = componentListEmitPhase2 source.Components
            return Phase2EquiData.Concat [x1; x2]
        }


    let itemListEmitPhase2 (source : S4Item list) : CompilerMonad<Phase2Data> = 
        mapM item1EmitPhase2 source |>> Phase2EquiData.Concat

    let assembly1EmitPhase2 (source : S4Assembly) : CompilerMonad<Phase2Data> = 
        compile { 
            let! x1 = equipmentsToPhase2EquiData source.FuncLoc source.FlocProperties source.Equipment
            let! x2 = itemListEmitPhase2 source.Items
            return Phase2EquiData.Concat [x1; x2]
        }


    let assemblyListEmitPhase2 (source : S4Assembly list) : CompilerMonad<Phase2Data> = 
        mapM assembly1EmitPhase2 source |>> Phase2EquiData.Concat
            

    let system1EmitPhase2 (source : S4System) : CompilerMonad<Phase2Data> = 
        compile { 
            let! x1 = equipmentsToPhase2EquiData source.FuncLoc source.FlocProperties source.Equipment
            let! x2 = assemblyListEmitPhase2 source.Assemblies
            return Phase2EquiData.Concat [x1; x2]
        }

    let systemListEmitPhase2 (source : S4System list) : CompilerMonad<Phase2Data> = 
        mapM system1EmitPhase2 source |>> Phase2EquiData.Concat
            
    let process1EmitPhase2 (source : S4Process) : CompilerMonad<Phase2Data> = 
        systemListEmitPhase2 source.Systems

    let processListEmitPhase2 (source : S4Process list) : CompilerMonad<Phase2Data> = 
        mapM process1EmitPhase2 source |>> Phase2EquiData.Concat 
        
    let processGroup1EmitPhase2 (source : S4ProcessGroup) : CompilerMonad<Phase2Data> = 
        processListEmitPhase2 source.Processes

    let processGroupListEmitPhase2 (source : S4ProcessGroup list) : CompilerMonad<Phase2Data> = 
        mapM processGroup1EmitPhase2 source |>> Phase2EquiData.Concat

    let function1EmitPhase2 (source : S4Function) : CompilerMonad<Phase2Data> = 
        processGroupListEmitPhase2 source.ProcessGroups

    let functionListEmitPhase2 (source : S4Function list) : CompilerMonad<Phase2Data> = 
        mapM function1EmitPhase2 source |>> Phase2EquiData.Concat

    let site1EmitPhase2 (source : S4Site) : CompilerMonad<Phase2Data> = 
        functionListEmitPhase2 source.Functions

    let siteListEmitPhase2 (source : S4Site list) : CompilerMonad<Phase2Data> = 
        mapM site1EmitPhase2 source |>> Phase2EquiData.Concat
