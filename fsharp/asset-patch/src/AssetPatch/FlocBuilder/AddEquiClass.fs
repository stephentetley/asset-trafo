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
    
    open AssetPatch.Base.EntityTypes
    open AssetPatch.FlocBuilder.Common
    open AssetPatch.FlocBuilder.Hierarchy
    

    let private makeValuaEqui (equiNumber : IntegerString) (count : int) (charac : Characteristic) : ValuaEqui = 
        { EquipmentNumber = equiNumber
          ClassType = IntegerString.OfString "002"
          CharacteristicID = charac.Name
          CharacteristicValue = charac.Value
          ValueCount = count
          Attributes = AssocList.empty
        }
    
    let private makeClassEqui (equiNumber : IntegerString)  (clazz : Class) : ClassEqui = 
        { EquipmentNumber = equiNumber
          Class = clazz.ClassName
          ClassType = IntegerString.OfString "002"
          ClassNumber = IntegerString.Create(10, clazz.ClassInt)
          Status = 1
          }
    
    
    
    let private makeEntities (equiNumber : IntegerString)  
                             (clazz : Class) : ClassEqui * ValuaEqui list =
        
        let makeGrouped (characs : Characteristic list) : ValuaEqui list = 
            characs |> List.mapi (fun i x -> makeValuaEqui equiNumber (i+1) x)

        let ce : ClassEqui = makeClassEqui equiNumber clazz
        let vs : ValuaEqui list  = 
            clazz.Characteritics 
                |> List.sortBy (fun x -> x.Name)
                |> List.groupBy (fun x -> x.Name)
                |> List.map (snd >> makeGrouped)
                |> List.concat
        (ce, vs)
    
    let makeAddPatches1 (classequiFile : string) 
                        (valuaequiFile : string) 
                        (user: string) 
                        (equiNumber : IntegerString)  
                        (clazz : Class) : CompilerMonad<unit, 'env, 'acc> = 
        compile {
            let (ce, vs) = makeEntities equiNumber clazz
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
        runCompiler () () 
            (makeAddPatches1 classequiFile valuaequiFile user equiNumber clazz)
                |> Result.map fst
