// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.PatchBuilder


module ClassAddPatcher =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.EntityTypes
    open AssetPatch.Base.CompilerMonad    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.PatchBuilder.Common
    open AssetPatch.PatchBuilder.Hierarchy
    open AssetPatch.PatchBuilder.Emitter
    



    let collectLists (xs : ('a * 'b list) list) : 'a list * 'b list = 
        List.foldBack (fun (c,vs1) (cs,vs) -> (c :: cs, vs1 @ vs)) xs ([],[])
    
    let applyTemplates (xs: ('id * 'b) list ) (template: 'b -> 'c) : ('id * 'c) list = 
        xs |> List.map (fun (name,x) -> (name, template x))


    let makeAddEquiPatches1 (classEquiFile : string) 
                            (valuaEquiFile : string) 
                            (user: string) 
                            (elements : (EquipmentCode * Class) list) : CompilerMonad<unit, 'env> = 
        compile {
            let! (cs, vs) = 
                forM elements (fun (x,c) -> makeEquiProperties1 x c) |>> collectLists
            let! classChanges = compileClassEquiFile user DateTime.Now cs
            let! valuaChanges = compileValuaEquiFile user DateTime.Now vs
            do! writeChangeFileAndMetadata classEquiFile classChanges
            do! writeChangeFileAndMetadata valuaEquiFile valuaChanges
            return ()
        }


    let makeAddEquiPatches (classequiFile : string) 
                            (valuaequiFile : string) 
                            (user: string) 
                            (src : (EquipmentCode * 'a) list)  
                            (template : 'a -> Class) : Result<unit, ErrMsg> = 
        let elements = applyTemplates src template
        runCompiler () 
            (makeAddEquiPatches1 classequiFile valuaequiFile user elements)


    let makeAddFlocPatches1 (classFlocFile : string) 
                            (valuaFlocFile : string) 
                            (user: string) 
                            (elements : (FuncLocPath * Class) list)  : CompilerMonad<unit, 'env> = 
        compile {
            let! (cs, vs) = 
                forM elements (fun (x,c) ->  makeFlocProperties1 x c) |>> collectLists
            let! classChanges = compileClassFlocFile user DateTime.Now cs
            let! valuaChanges = compileValuaFlocFile user DateTime.Now vs
            do! writeChangeFileAndMetadata classFlocFile classChanges
            do! writeChangeFileAndMetadata valuaFlocFile valuaChanges
            return ()
        }

    let makeAddFlocPatches (classFlocFile : string) 
                            (valuaFlocFile : string) 
                            (user: string) 
                            (src : (FuncLocPath * 'a) list)  
                            (template : 'a -> Class) : Result<unit, ErrMsg> = 
        let elements = applyTemplates src template
        runCompiler () 
            (makeAddFlocPatches1 classFlocFile valuaFlocFile user elements)


