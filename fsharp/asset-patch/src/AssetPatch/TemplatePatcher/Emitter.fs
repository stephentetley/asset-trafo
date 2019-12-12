// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module Emitter =

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.EmitEquipment
    open AssetPatch.TemplatePatcher.EmitFuncLoc
    

    let private collect (xs : (FuncLocResult1 * EquiResults) list) : FuncLocResults * EquiResults = 
        let flocResults = xs|> List.map fst |> collectFuncLocResults
        let equiResults = xs|> List.map snd |> concatEquiResults
        (flocResults, equiResults)

    let equipmentEmit (flocPath : FuncLocPath) 
                        (props : FuncLocProperties)
                        (source : S4Equipment) : CompilerMonad<EquiResult1 list> = 
        let rec work kids cont = 
            match kids with
            | [] -> mreturn []
            | (x :: xs) -> 
                compile {
                    let! v1 = equipmentToEquiResult1 flocPath props x
                    return! work kids (fun vs -> let ans = (v1 :: vs) in cont ans)
                }
        compile {
            let! equiResult1 = equipmentToEquiResult1 flocPath props source
            let! kids = work source.SuboridnateEquipment id
            return (equiResult1 :: kids)
        }

    let equipmentsEmit (flocPath : FuncLocPath) (props : FuncLocProperties) (source : S4Equipment list) : CompilerMonad<EquiResults> = 
        mapM (equipmentEmit flocPath props) source |>> (List.concat >> collectEquiResults)

    let private componentEmit (source : S4Component) : CompilerMonad<FuncLocResult1 * EquiResults> = 
        compile {
            let! flocResult = 
                funclocToFuncLocResult1 source.FuncLoc source.InterimId source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = equipmentsEmit source.FuncLoc source.FlocProperties source.Equipment
            return (flocResult, equiResults)
        }

    let componentsEmit (source : S4Component list) : CompilerMonad<FuncLocResults * EquiResults> = 
        mapM componentEmit source |>> collect


    let private itemEmit (source : S4Item) : CompilerMonad<FuncLocResult1 * EquiResults> = 
        compile {
            let! flocResult = 
                funclocToFuncLocResult1 source.FuncLoc source.InterimId source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = equipmentsEmit source.FuncLoc source.FlocProperties source.Equipment
            return (flocResult, equiResults)
        }

    let itemsEmit (source : S4Item list) : CompilerMonad<FuncLocResults * EquiResults> = 
        mapM itemEmit source |>> collect

    let assemblyEmit (source : S4Assembly) : CompilerMonad<FuncLocResult1 * EquiResults> = 
        compile {
            let! flocResult = 
                funclocToFuncLocResult1 source.FuncLoc source.InterimId source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = equipmentsEmit source.FuncLoc source.FlocProperties source.Equipment
            return (flocResult, equiResults)
        }

    let assembliesEmit (source : S4Assembly list) : CompilerMonad<FuncLocResults * EquiResults> = 
        mapM assemblyEmit source |>> collect

    let systemEmit (source : S4System) : CompilerMonad<FuncLocResult1 * EquiResults> = 
        compile {
            let! flocResult = 
                funclocToFuncLocResult1 source.FuncLoc source.InterimId source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiResults = equipmentsEmit source.FuncLoc source.FlocProperties source.Equipment
            return (flocResult, equiResults)
        }

    let systemsEmit (source : S4System list) : CompilerMonad<FuncLocResults * EquiResults> = 
        mapM systemEmit source |>> collect


    let processEmit (source : S4Process) : CompilerMonad<FuncLocResult1> = 
        funclocToFuncLocResult1 source.FuncLoc source.InterimId source.FlocProperties source.Description source.ObjectType source.Classes
        

    let processesEmit (source : S4Process list) : CompilerMonad<FuncLocResults> = 
        mapM processEmit source |>> collectFuncLocResults


    let processGroupEmit (source : S4ProcessGroup) : CompilerMonad<FuncLocResult1> = 
        funclocToFuncLocResult1 source.FuncLoc source.InterimId source.FlocProperties source.Description source.ObjectType source.Classes
        

    let processGroupsEmit (source : S4ProcessGroup list) : CompilerMonad<FuncLocResults> = 
        mapM processGroupEmit source |>> collectFuncLocResults


    let functionEmit (source : S4Function) : CompilerMonad<FuncLocResult1> = 
        funclocToFuncLocResult1 source.FuncLoc source.InterimId source.FlocProperties source.Description source.ObjectType source.Classes
        

    let functionsEmit (source : S4Function list) : CompilerMonad<FuncLocResults> = 
        mapM functionEmit source |>> collectFuncLocResults

        
    let siteEmit (source : S4Site) : CompilerMonad<FuncLocResult1> = 
        funclocToFuncLocResult1 source.FuncLoc source.InterimId source.FlocProperties source.Description source.ObjectType source.Classes
            

    let sitesEmit (source : S4Site list) : CompilerMonad<FuncLocResults> = 
        mapM siteEmit source |>> collectFuncLocResults

    let classesEmit (source : (string * S4Class list) list) : CompilerMonad<ClassEquiInstances> = 
        source 
            |> mapM (fun (equipId, classes) -> equipmentToEquiProperties equipId classes) 
            |>> collectClassEquiInstances