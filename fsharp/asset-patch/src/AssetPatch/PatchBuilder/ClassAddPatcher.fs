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
    open AssetPatch.PatchBuilder.BuildCommon
    



    let collectLists (xs : ('a * 'b list) list) : 'a list * 'b list = 
        List.foldBack (fun (c,vs1) (cs,vs) -> (c :: cs, vs1 @ vs)) xs ([],[])
    

    let makeAddEquiPatches1 (classEquiFile : string) 
                            (valuaEquiFile : string) 
                            (user: string) 
                            (equiNumbers : IntegerString list)  
                            (clazz : Class) : CompilerMonad<unit, 'env> = 
        compile {
            let (cs, vs) = 
                equiNumbers |> List.map (fun x ->  makeEquiAttributes x clazz) |> collectLists
            let! classChanges = compileClassEquiFile user DateTime.Now cs
            let! valuaChanges = compileValuaEquiFile user DateTime.Now vs
            do! writeChangeFileAndMetadata classEquiFile classChanges
            do! writeChangeFileAndMetadata valuaEquiFile valuaChanges
            return ()
        }


    let makeAddEquiPatches (classequiFile : string) 
                            (valuaequiFile : string) 
                            (user: string) 
                            (equiNumbers : IntegerString list)  
                            (clazz : Class) : Result<unit, ErrMsg> = 
        runCompiler () 
            (makeAddEquiPatches1 classequiFile valuaequiFile user equiNumbers clazz)


    let makeAddFlocPatches1 (classFlocFile : string) 
                            (valuaFlocFile : string) 
                            (user: string) 
                            (funcLocs : FuncLocPath list)  
                            (clazz : Class) : CompilerMonad<unit, 'env> = 
        compile {
            let (cs, vs) = 
                funcLocs |> List.map (fun x ->  makeFlocAttributes x clazz) |> collectLists
            let! classChanges = compileClassFlocFile user DateTime.Now cs
            let! valuaChanges = compileValuaFlocFile user DateTime.Now vs
            do! writeChangeFileAndMetadata classFlocFile classChanges
            do! writeChangeFileAndMetadata valuaFlocFile valuaChanges
            return ()
        }

    let makeAddFlocPatches (classFlocFile : string) 
                            (valuaFlocFile : string) 
                            (user: string) 
                            (funcLocs : FuncLocPath list)  
                            (clazz : Class) : Result<unit, ErrMsg> = 
        runCompiler () 
            (makeAddFlocPatches1 classFlocFile valuaFlocFile user funcLocs clazz)


