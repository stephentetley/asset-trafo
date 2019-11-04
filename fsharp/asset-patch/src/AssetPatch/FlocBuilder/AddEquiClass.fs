// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocBuilder


module AddEquiClass =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.EntityTypes
    open AssetPatch.Base.CompilerMonad
    
    open AssetPatch.FlocBuilder.Common
    open AssetPatch.FlocBuilder.Hierarchy
    open AssetPatch.FlocBuilder.BuildCommon
    


    
    let makeAddPatches1 (classequiFile : string) 
                        (valuaequiFile : string) 
                        (user: string) 
                        (equiNumber : IntegerString)  
                        (clazz : Class) : CompilerMonad<unit, 'env> = 
        compile {
            let (ce, vs) = makeEquiAttributes equiNumber clazz
            let! classChanges = compileClassEquiFile user DateTime.Now [ce]
            let! valuaChanges = compileValuaEquiFile user DateTime.Now vs
            do! writeChangeFileAndMetadata classequiFile classChanges
            do! writeChangeFileAndMetadata valuaequiFile valuaChanges
            return ()
        }

    let makeAddPatches (classequiFile : string) 
                        (valuaequiFile : string) 
                        (user: string) 
                        (equiNumber : IntegerString)  
                        (clazz : Class) : Result<unit, ErrMsg> = 
        runCompiler () 
            (makeAddPatches1 classequiFile valuaequiFile user equiNumber clazz)

