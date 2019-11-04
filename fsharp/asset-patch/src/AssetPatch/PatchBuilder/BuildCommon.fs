// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.PatchBuilder


module BuildCommon =

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.EntityTypes
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.PatchBuilder.Hierarchy
    
    let characteristicToValuaFloc (funcLoc : FuncLocPath) 
                                    (count : int) (charac : Characteristic) : ValuaFloc = 
        { FuncLoc = funcLoc
          ClassType = IntegerString.OfString "003"
          CharacteristicID = charac.Name
          CharacteristicValue = charac.Value
          ValueCount = count
          Attributes = AssocList.empty
        }

    let characteristicToValuaEqui (equiNumber : IntegerString) 
                                    (count : int) (charac : Characteristic) : ValuaEqui = 
        { EquipmentNumber = equiNumber
          ClassType = IntegerString.OfString "002"
          CharacteristicID = charac.Name
          CharacteristicValue = charac.Value
          ValueCount = count
          Attributes = AssocList.empty
        }
    

    let classToClassFloc (funcLoc : FuncLocPath)  (clazz : Class) : ClassFloc = 
        { FuncLoc = funcLoc
          Class = clazz.ClassName
          ClassType = IntegerString.OfString "003"
          ClassNumber = IntegerString.Create(10, clazz.ClassInt)
          Status = 1
          }


    let classToClassEqui (equiNumber : IntegerString)  (clazz : Class) : ClassEqui = 
        { EquipmentNumber = equiNumber
          Class = clazz.ClassName
          ClassType = IntegerString.OfString "002"
          ClassNumber = IntegerString.Create(10, clazz.ClassInt)
          Status = 1
          }
    
    let equipmentToEqui1 (funcLoc : FuncLocPath) (equipment: Equipment) : Equi = 
         { EquipmentNumber = IntegerString.OfString("*MAGIC*")
           Description = equipment.Description
           FuncLoc = funcLoc
           Attributes = AssocList.empty
         }
    
    let equipmentToEquis (funcLoc : FuncLocPath) (equipment: Equipment) : Equi list = 
        let rec work (kids : Equipment list) cont = 
            match kids with
            | [] -> cont []
            | x :: xs -> 
                let e1 = equipmentToEqui1 funcLoc x
                work x.SuboridnateEquipment (fun vs1 -> 
                work xs (fun vs2 -> 
                cont (e1 :: vs1 @ vs2)))
        let e1 = equipmentToEqui1 funcLoc equipment
        work equipment.SuboridnateEquipment (fun es -> e1 :: es)

    type FlocClassProperties = ClassFloc * ValuaFloc list
    
    type EquiClassProperties = ClassEqui * ValuaEqui list


    let makeFlocProperties1 (funcLoc : FuncLocPath)  
                            (clazz : Class) : FlocClassProperties =
        
        let makeGrouped (chars : Characteristic list) : ValuaFloc list = 
            chars |> List.mapi (fun i x -> characteristicToValuaFloc funcLoc (i+1) x)

        let ce : ClassFloc = classToClassFloc funcLoc clazz
        let vs : ValuaFloc list  = 
            clazz.Characteritics 
                |> List.sortBy (fun x -> x.Name)
                |> List.groupBy (fun x -> x.Name)               
                |> List.map (snd >> makeGrouped)
                |> List.concat
        (ce, vs)

    let makeEquiProperties1 (equiNumber : IntegerString)  
                            (clazz : Class) : EquiClassProperties =
        
        let makeGrouped (chars : Characteristic list) : ValuaEqui list = 
            chars |> List.mapi (fun i x -> characteristicToValuaEqui equiNumber (i+1) x)

        let ce : ClassEqui = classToClassEqui equiNumber clazz
        let vs : ValuaEqui list  = 
            clazz.Characteritics 
                |> List.sortBy (fun x -> x.Name)
                |> List.groupBy (fun x -> x.Name)               
                |> List.map (snd >> makeGrouped)
                |> List.concat
        (ce, vs)
    
    //let equipmentEquiProperties1 (equipment: Equipment) : EquiClassProperties list = 
    //    makeEquiProperties1 equipment.
