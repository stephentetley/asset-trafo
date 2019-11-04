// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocBuilder


module BuildCommon =

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.EntityTypes
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.FlocBuilder.Hierarchy
    
    let makeValuaFloc (funcLoc : FuncLocPath) 
                        (count : int) (charac : Characteristic) : ValuaFloc = 
        { FuncLoc = funcLoc
          ClassType = IntegerString.OfString "003"
          CharacteristicID = charac.Name
          CharacteristicValue = charac.Value
          ValueCount = count
          Attributes = AssocList.empty
        }

    let makeValuaEqui (equiNumber : IntegerString) 
                        (count : int) (charac : Characteristic) : ValuaEqui = 
        { EquipmentNumber = equiNumber
          ClassType = IntegerString.OfString "002"
          CharacteristicID = charac.Name
          CharacteristicValue = charac.Value
          ValueCount = count
          Attributes = AssocList.empty
        }
    

    let makeClassFloc (funcLoc : FuncLocPath)  (clazz : Class) : ClassFloc = 
        { FuncLoc = funcLoc
          Class = clazz.ClassName
          ClassType = IntegerString.OfString "003"
          ClassNumber = IntegerString.Create(10, clazz.ClassInt)
          Status = 1
          }


    let makeClassEqui (equiNumber : IntegerString)  (clazz : Class) : ClassEqui = 
        { EquipmentNumber = equiNumber
          Class = clazz.ClassName
          ClassType = IntegerString.OfString "002"
          ClassNumber = IntegerString.Create(10, clazz.ClassInt)
          Status = 1
          }
    
    
    
    let makeFlocAttributes (funcLoc : FuncLocPath)  
                            (clazz : Class) : ClassFloc * ValuaFloc list =
        
        let makeGrouped (chars : Characteristic list) : ValuaFloc list = 
            chars |> List.mapi (fun i x -> makeValuaFloc funcLoc (i+1) x)

        let ce : ClassFloc = makeClassFloc funcLoc clazz
        let vs : ValuaFloc list  = 
            clazz.Characteritics 
                |> List.sortBy (fun x -> x.Name)
                |> List.groupBy (fun x -> x.Name)               
                |> List.map (snd >> makeGrouped)
                |> List.concat
        (ce, vs)

    let makeEquiAttributes (equiNumber : IntegerString)  
                            (clazz : Class) : ClassEqui * ValuaEqui list =
        
        let makeGrouped (chars : Characteristic list) : ValuaEqui list = 
            chars |> List.mapi (fun i x -> makeValuaEqui equiNumber (i+1) x)

        let ce : ClassEqui = makeClassEqui equiNumber clazz
        let vs : ValuaEqui list  = 
            clazz.Characteritics 
                |> List.sortBy (fun x -> x.Name)
                |> List.groupBy (fun x -> x.Name)               
                |> List.map (snd >> makeGrouped)
                |> List.concat
        (ce, vs)
    
