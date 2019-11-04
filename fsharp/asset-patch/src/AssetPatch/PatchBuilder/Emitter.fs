// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.PatchBuilder


module Emitter =

    open AssetPatch.Base
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.EntityTypes
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.PatchBuilder.Hierarchy
    
    type FlocClassProperties = ClassFloc * ValuaFloc list
    
    type EquiClassProperties = ClassEqui * ValuaEqui list


    let characteristicToValuaFloc (funcLoc : FuncLocPath) 
                                    (count : int) 
                                    (charac : Characteristic) : CompilerMonad<ValuaFloc, 'env> = 
        mreturn {   
            FuncLoc = funcLoc
            ClassType = IntegerString.OfString "003"
            CharacteristicID = charac.Name
            CharacteristicValue = charac.Value
            ValueCount = count
            Attributes = AssocList.empty
        }

    let characteristicToValuaEqui (equiNumber : EquipmentCode) 
                                    (count : int) 
                                    (charac : Characteristic) : CompilerMonad<ValuaEqui, 'env> = 
        mreturn { 
            EquipmentNumber = equiNumber
            ClassType = IntegerString.OfString "002"
            CharacteristicID = charac.Name
            CharacteristicValue = charac.Value
            ValueCount = count
            Attributes = AssocList.empty
        }
    

    let classToClassFloc (funcLoc : FuncLocPath)  (clazz : Class) : CompilerMonad<ClassFloc, 'env> = 
        mreturn { 
            FuncLoc = funcLoc
            Class = clazz.ClassName
            ClassType = IntegerString.OfString "003"
            ClassNumber = IntegerString.Create(10, clazz.ClassInt)
            Status = 1
        }


    let classToClassEqui (equiNumber : EquipmentCode)
                         (clazz : Class) : CompilerMonad<ClassEqui, 'env> = 
        mreturn { 
            EquipmentNumber = equiNumber
            Class = clazz.ClassName
            ClassType = IntegerString.OfString "002"
            ClassNumber = IntegerString.Create(10, clazz.ClassInt)
            Status = 1
        }
    
    let equipmentToEqui1 (funcLoc : FuncLocPath) 
                         (equipment: Equipment) : CompilerMonad<Equi, 'env> = 
        compile {
            let! number = newEquipmentName ()
            return { 
                EquipmentNumber = EquipmentMagic number
                Description = equipment.Description
                FuncLoc = funcLoc
                Attributes = AssocList.empty
            }
        }
        

    let equipmentToEquis (funcLoc : FuncLocPath) 
                         (equipment: Equipment) : CompilerMonad<Equi list, 'env> = 
        let rec work (kids : Equipment list) 
                     (cont : Equi list -> CompilerMonad<Equi list, 'env>) = 
            match kids with
            | [] -> cont []
            | x :: xs -> 
                compile { 
                    let! e1 = equipmentToEqui1 funcLoc x
                    return! work x.SuboridnateEquipment (fun vs1 -> 
                            work xs (fun vs2 -> 
                            cont (e1 :: vs1 @ vs2)))
                } 
        compile { 
            let! e1 = equipmentToEqui1 funcLoc equipment
            return! work equipment.SuboridnateEquipment (fun es -> mreturn (e1 :: es))
        }

    


    



    let sortedCharacteristics (clazz : Class) : (Characteristic list) list= 
        clazz.Characteritics 
            |> List.sortBy (fun x -> x.Name)
            |> List.groupBy (fun x -> x.Name)               
            |> List.map snd


    let makeFlocProperties1 (funcLoc : FuncLocPath)  
                            (clazz : Class) : CompilerMonad<FlocClassProperties, 'env> =
        
        let makeGrouped (chars : Characteristic list) : CompilerMonad<ValuaFloc list, 'env> = 
            foriM chars (fun i x -> characteristicToValuaFloc funcLoc (i+1) x)

        compile {
            let! cf = classToClassFloc funcLoc clazz
            let chars = sortedCharacteristics clazz
            let! vs = mapM makeGrouped chars |>> List.concat
            return (cf, vs)
        } 

    let makeEquiProperties1 (equiNumber : EquipmentCode)  
                            (clazz : Class) : CompilerMonad<EquiClassProperties, 'env> =
        
        let makeGrouped (chars : Characteristic list) : CompilerMonad<ValuaEqui list, 'env> = 
            foriM chars (fun i x -> characteristicToValuaEqui equiNumber (i+1) x)

        compile {
            let! ce = classToClassEqui equiNumber clazz
            let chars = sortedCharacteristics clazz
            let! vs = mapM makeGrouped chars |>> List.concat
            return (ce, vs)
        } 
    
    let equipmentClassProperties1 (code: EquipmentCode) 
                                  (equipment: Equipment) : CompilerMonad<EquiClassProperties list, 'env> = 
        forM equipment.Classes (makeEquiProperties1 code)

    //let equipmentClassProperties (code: EquipmentCode)
    //                             (equipment: Equipment) : CompilerMonad<EquiClassProperties list, 'env> = 
    //    let rec work (kids : Equipment list) 
    //                 (cont : Equi list -> CompilerMonad<EquiClassProperties list, 'env>) = 
    //        match kids with
    //        | [] -> cont []
    //        | x :: xs -> 
    //            compile { 
    //                let! e1 = makeEquiProperties1 x
    //                return! work x.SuboridnateEquipment (fun vs1 -> 
    //                        work xs (fun vs2 -> 
    //                        cont (e1 :: vs1 @ vs2)))
    //            } 
    //    compile { 
    //        let! e1 = makeEquiProperties1 equipment
    //        return! work equipment.SuboridnateEquipment (fun es -> mreturn (e1 :: es))
    //    }
