// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module EmitEquipment =

    open System.IO

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.Hierarchy
    open AssetPatch.TemplatePatcher.EquiIndexing
    open AssetPatch.TemplatePatcher.PatchWriter
    
    
    
    type ClassEquiInstances = 
        { ClassEquis : PatchClassEqui list
          ValuaEquis : PatchValuaEqui list
        }
        member x.IsEmpty 
            with get () : bool = 
                x.ClassEquis.IsEmpty && x.ValuaEquis.IsEmpty

    let collectClassEquiInstances (source : ClassEquiInstances list) : ClassEquiInstances = 
        let add (r1 : ClassEquiInstances) (acc : ClassEquiInstances) = 
            { ClassEquis = r1.ClassEquis @ acc.ClassEquis
              ValuaEquis = r1.ValuaEquis @ acc.ValuaEquis
            }
        List.foldBack add source { ClassEquis = []; ValuaEquis = [] }


    type EquiResult1 = 
        { Equi : PatchEqui
          ClassEquis : PatchClassEqui list
          ValuaEquis : PatchValuaEqui list
        }

    type EquiResults = 
        { Equis : PatchEqui list
          ClassEquis : PatchClassEqui list
          ValuaEquis : PatchValuaEqui list
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
                                    (charac : S4Characteristic) : CompilerMonad<PatchValuaEqui> = 
        mreturn { 
            EquipmentNumber = equiNumber
            ClassType = IntegerString.OfString "002"
            CharacteristicID = charac.Name
            CharacteristicValue = charac.Value
            ValueCount = count
        }
    

    let classToClassEqui (equiNumber : string)
                         (clazz : S4Class) : CompilerMonad<PatchClassEqui> = 
        mreturn { 
            EquipmentNumber = equiNumber
            Class = clazz.ClassName
            ClassType = IntegerString.OfString "002"
            ClassNumber = IntegerString.Create(10, clazz.ClassInt)
            Status = 1
        }



    let translateS4CharacteristicsEqui (equiNumber : string)
                                        (characteristics : S4Characteristic list) : CompilerMonad<PatchValuaEqui list> =  

        let makeGrouped (chars : S4Characteristic list) : CompilerMonad<PatchValuaEqui list> = 
            foriM chars (fun i x -> characteristicToValuaEqui equiNumber (i+1) x)

        compile {
            let chars = sortedCharacteristics characteristics
            return! mapM makeGrouped chars |>> List.concat
        }


    let translateS4ClassEqui (equiNumber : string)
                                   (clazz : S4Class) : CompilerMonad<PatchClassEqui * PatchValuaEqui list> = 
           compile {
               let! ce = classToClassEqui equiNumber clazz
               let! vs = translateS4CharacteristicsEqui equiNumber clazz.Characteristics
               return (ce, vs)
           }

    
    let equipmentToEqui1 (funcLoc : FuncLocPath) 
                            (props : FuncLocProperties)
                            (equipment : S4Equipment) : CompilerMonad<PatchEqui> = 
        let commonProps : CommonProperties = 
            { CompanyCode = props.CompanyCode 
              ControllingArea = props.ControllingArea 
              PlantCode = props.MaintenancePlant
              UserStatus = props.ObjectStatus
            }

        compile {
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
                StartupDate = props.StartupDate
                ConstructionYear = 
                     Option.defaultValue (uint16 props.StartupDate.Year) equipment.ConstructionYear
                ConstructionMonth = 
                    Option.defaultValue (uint8 props.StartupDate.Month) equipment.ConstructionMonth
                MaintenancePlant = props.MaintenancePlant
                Currency = props.Currency
                CommonProps = commonProps
            }
        }
        

    let equipmentToEquiResult1 (funcLoc : FuncLocPath) 
                                (props : FuncLocProperties)
                                (equipment1 : S4Equipment) : CompilerMonad<EquiResult1> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile { 
            let! e1 = equipmentToEqui1 funcLoc props equipment1
            let enumber = e1.EquipmentNumber
            let! (cs, vs) = mapM (translateS4ClassEqui enumber) equipment1.Classes |>> collect
            return { 
                Equi = e1
                ClassEquis = cs
                ValuaEquis = vs
            }
        } 

    let equipmentToEquiProperties (equiNumber : string) 
                                    (classes : S4Class list) : CompilerMonad<ClassEquiInstances> = 
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
                            (equiInstances : ClassEquiInstances) : CompilerMonad<unit> = 
        if equiInstances.IsEmpty then
            mreturn ()
        else
            compile {
                do! writeClassEquiFile directory level filePrefix equiInstances.ClassEquis
                do! writeValuaEquiFile directory level filePrefix equiInstances.ValuaEquis
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
    