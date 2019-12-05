// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module EmitEquipment =

    open System.IO

    open AssetPatch.Base
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.Hierarchy
    open AssetPatch.TemplatePatcher.EquiIndexing
    open AssetPatch.TemplatePatcher.PatchWriter
    
    
    
    type EquiProperties = 
        { ClassEquis : ClassEqui list
          ValuaEquis : ValuaEqui list
        }
        member x.IsEmpty 
            with get () : bool = 
                x.ClassEquis.IsEmpty && x.ValuaEquis.IsEmpty

    let collectEquiProperties (source : EquiProperties list) : EquiProperties = 
        let add (r1 : EquiProperties) (acc : EquiProperties) = 
            { ClassEquis = r1.ClassEquis @ acc.ClassEquis
              ValuaEquis = r1.ValuaEquis @ acc.ValuaEquis
            }
        List.foldBack add source { ClassEquis = []; ValuaEquis = [] }


    type EquiResult1 = 
        { Equi : Equi
          ClassEquis : ClassEqui list
          ValuaEquis : ValuaEqui list
        }

    type EquiResults = 
        { Equis : Equi list
          ClassEquis : ClassEqui list
          ValuaEquis : ValuaEqui list
        }
        member x.IsEmpty 
            with get () : bool = 
                x.Equis.IsEmpty &&  x.ClassEquis.IsEmpty && x.ValuaEquis.IsEmpty
            

    let collectEquiResults (source : EquiResult1 list) : EquiResults = 
        let add (r1 : EquiResult1) (acc : EquiResults) = 
            { Equis = r1.Equi :: acc.Equis
              ClassEquis = r1.ClassEquis @ acc.ClassEquis
              ValuaEquis = r1.ValuaEquis @ acc.ValuaEquis
            }
        List.foldBack add source { Equis = []; ClassEquis = []; ValuaEquis = [] }

    let concatEquiResults (source : EquiResults list) : EquiResults = 
        let add (r1 : EquiResults) (acc : EquiResults) = 
            { Equis = r1.Equis @ acc.Equis
              ClassEquis = r1.ClassEquis @ acc.ClassEquis
              ValuaEquis = r1.ValuaEquis @ acc.ValuaEquis
            }
        List.foldBack add source { Equis = []; ClassEquis = []; ValuaEquis = [] }

    let characteristicToValuaEqui (equiNumber : string) 
                                    (count : int) 
                                    (charac : S4Characteristic) : CompilerMonad<ValuaEqui> = 
        mreturn { 
            EquipmentNumber = equiNumber
            ClassType = IntegerString.OfString "002"
            CharacteristicID = charac.Name
            CharacteristicValue = charac.Value
            ValueCount = count
        }
    

    let classToClassEqui (equiNumber : string)
                         (clazz : S4Class) : CompilerMonad<ClassEqui> = 
        mreturn { 
            EquipmentNumber = equiNumber
            Class = clazz.ClassName
            ClassType = IntegerString.OfString "002"
            ClassNumber = IntegerString.Create(10, clazz.ClassInt)
            Status = 1
        }



    let translateS4CharacteristicsEqui (equiNumber : string)
                                        (characteristics : S4Characteristic list) : CompilerMonad<ValuaEqui list> =  

        let makeGrouped (chars : S4Characteristic list) : CompilerMonad<ValuaEqui list> = 
            foriM chars (fun i x -> characteristicToValuaEqui equiNumber (i+1) x)

        compile {
            let chars = sortedCharacteristics characteristics
            return! mapM makeGrouped chars |>> List.concat
        }


    let translateS4ClassEqui (equiNumber : string)
                                   (clazz : S4Class) : CompilerMonad<ClassEqui * ValuaEqui list> = 
           compile {
               let! ce = classToClassEqui equiNumber clazz
               let! vs = translateS4CharacteristicsEqui equiNumber clazz.Characteristics
               return (ce, vs)
           }

    
    let equipmentToEqui1 (funcLoc : FuncLocPath) 
                         (equipment : S4Equipment) : CompilerMonad<Equi> = 
        compile {
            let! sdate = asks (fun x -> x.StartupDate)
            let! mplant = asks (fun x -> x.MaintenancePlant)
            return { 
                EquipmentNumber = equipment.EquipmentId
                Description = equipment.Description
                FuncLoc = funcLoc
                Category = equipment.Category
                ObjectType = equipment.ObjectType
                Manufacturer = 
                    Option.defaultValue "TO BE DETERMINED" equipment.Manufacturer
                Model = Option.defaultValue "TO BE DETERMINED" equipment.Model
                SerialNumber = Option.defaultValue "" equipment.SerialNumber
                StartupDate = sdate
                ConstructionYear = 
                     Option.defaultValue (uint16 sdate.Year) equipment.ConstructionYear
                ConstructionMonth = 
                    Option.defaultValue (uint8 sdate.Month) equipment.ConstructionMonth
                MaintenancePlant = mplant
            }
        }
        

    let equipmentToEquiResult1 (funcLoc : FuncLocPath) 
                                (equipment1 : S4Equipment) : CompilerMonad<EquiResult1> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile { 
            let! e1 = equipmentToEqui1 funcLoc equipment1
            let enumber = e1.EquipmentNumber
            let! (cs, vs) = mapM (translateS4ClassEqui enumber) equipment1.Classes |>> collect
            return { 
                Equi = e1
                ClassEquis = cs
                ValuaEquis = vs
            }
        } 

    let equipmentToEquiProperties (equiNumber : string) 
                                    (classes : S4Class list) : CompilerMonad<EquiProperties> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile { 
            let! (cs, vs) = mapM (translateS4ClassEqui equiNumber) classes |>> collect
            return { 
                ClassEquis = cs
                ValuaEquis = vs
            }
        } 


    
    // ************************************************************************
    // Write output

    let writeEquiProperties (directory : string) 
                            (level : int)
                            (filePrefix : string) 
                            (equiProperties : EquiProperties) : CompilerMonad<unit> = 
        if equiProperties.IsEmpty then
            mreturn ()
        else
            compile {
                do! writeClassEquiFile directory level filePrefix equiProperties.ClassEquis
                do! writeValuaEquiFile directory level filePrefix equiProperties.ValuaEquis
                return ()
            }

    let writeEquiResults (directory : string) 
                            (level : int)
                            (filePrefix : string) 
                            (equiResults : EquiResults) : CompilerMonad<unit> = 
        if equiResults.IsEmpty then
            mreturn ()
        else
            compile {
                let! dirName = genSubFolder directory level
                let equiFile = Path.Combine(dirName, "EquiIndexing.xlsx")
                do! writeEquiIndexingSheet equiFile equiResults.Equis
                do! writeEquiFile directory level filePrefix equiResults.Equis
                do! writeClassEquiFile directory level filePrefix equiResults.ClassEquis
                do! writeValuaEquiFile directory level filePrefix equiResults.ValuaEquis
                return ()
            }
    