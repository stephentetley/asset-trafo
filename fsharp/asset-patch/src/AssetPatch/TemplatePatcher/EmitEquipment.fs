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
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.PatchWriter
    open AssetPatch.TemplatePatcher.EmitCommon
    

    let private characteristicToNewValuaEqui1 (equiId : uint32) 
                                                (count : int) 
                                                (charac : S4Characteristic) : CompilerMonad<NewValuaEqui> = 
        mreturn { 
            EquipmentId = equiId
            ClassType = IntegerString.OfString "002"
            CharacteristicID = charac.Name
            ValueCount = count
            Value = charac.Value
        }
    

    let private classToNewClassEqui (equiId : uint32)
                                    (s4Class : S4Class) : CompilerMonad<NewClassEqui> = 
        mreturn { 
            EquipmentId = equiId
            Class = s4Class.ClassName
            Status = 1
        }



    let private characteristicsToNewValuaEquis (equiId : uint32)
                                               (characteristics : S4Characteristic list) : CompilerMonad<NewValuaEqui list> =  

        let makeGrouped (chars : S4Characteristic list) : CompilerMonad<NewValuaEqui list> = 
            foriM chars (fun i x -> characteristicToNewValuaEqui1 equiId (i+1) x)

        compile {
            let chars = sortedCharacteristics characteristics
            return! mapM makeGrouped chars |>> List.concat
        }


    let private classToProperties (equiId : uint32)
                                    (clazz : S4Class) : CompilerMonad<NewClassEqui * NewValuaEqui list> = 
           compile {
               let! ce = classToNewClassEqui equiId clazz
               let! vs = characteristicsToNewValuaEquis equiId clazz.Characteristics
               return (ce, vs)
           }

    
    let private equipmentToNewEqui1 (funcLoc : FuncLocPath) 
                                    (props : FuncLocProperties)
                                    (equipment : S4Equipment) : CompilerMonad<NewEqui> = 
        let commonProps : CommonProperties = 
            { CompanyCode = props.CompanyCode 
              ControllingArea = props.ControllingArea 
              PlantCode = props.MaintenancePlant
              UserStatus = props.ObjectStatus
            }

        compile {
            return { 
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
        
    /// Recursive version of equipmentToNewEqui1
    let equipmentToNewEqui (flocPath : FuncLocPath) 
                        (props : FuncLocProperties)
                        (source : S4Equipment) : CompilerMonad<NewEqui list> = 
            let rec work kids cont = 
                match kids with
                | [] -> mreturn []
                | (x :: xs) -> 
                    compile {
                        let! v1 = equipmentToNewEqui1 flocPath props x
                        return! work kids (fun vs -> let ans = (v1 :: vs) in cont ans)
                    }
            compile {
                let! equiResult1 = equipmentToNewEqui1 flocPath props source
                let! kids = work source.SuboridnateEquipment id
                return (equiResult1 :: kids)
            }

    let equipmentsToPhase1EquiData (flocPath : FuncLocPath) 
                            (props : FuncLocProperties) 
                            (source : S4Equipment list) : CompilerMonad<Phase1EquiData> = 
        compile { 
            let! xss = mapM (equipmentToNewEqui flocPath props) source 
            return { Equis = List.concat xss }
        }


    let equipmentToPhase2EquiData1 (equiId : uint32) 
                                     (classes : S4Class list) : CompilerMonad<Phase2EquiData> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile { 
            let! (cs, vs) = mapM (classToProperties equiId) classes |>> collect
            return { 
                ClassEquis = cs
                ValuaEquis = vs
            }
        } 

    /// Recursive version of equipmentToNewEqui1
    let equipmentToPhase2EquiData (flocPath : FuncLocPath) 
                                        (source : S4Equipment) : CompilerMonad<Phase2EquiData> = 
            let rec work (kids : S4Equipment list) cont = 
                match kids with
                | [] -> mreturn []
                | (x :: xs) -> 
                    compile {
                        match x.EquipmentId with 
                        | None -> return! throwError (sprintf "Missing equipment for %s '%s'" (flocPath.ToString()) source.Description)
                        | Some equiNum -> 
                            let! v1 = equipmentToPhase2EquiData1 equiNum x.Classes
                            return! work kids (fun vs -> let ans = (v1 :: vs) in cont ans)
                    }

            compile {
                match source.EquipmentId with
                | None -> return! throwError (sprintf "Missing equipment for %s '%s'" (flocPath.ToString()) source.Description)
                | Some equiNum -> 
                    let! equiResult1 = equipmentToPhase2EquiData1 equiNum source.Classes
                    let! kids = work source.SuboridnateEquipment id
                    return (equiResult1 :: kids |> concatPhase2EquiData)
            }
    
    let equipmentsToPhase2EquiData (flocPath : FuncLocPath) 
                                    (props : FuncLocProperties) 
                                    (source : S4Equipment list) : CompilerMonad<Phase2EquiData> = 
        compile { 
            let! xss = mapM (equipmentToPhase2EquiData flocPath) source 
            return (concatPhase2EquiData xss)
        }

    // ************************************************************************
    // Write output
    
    // Write an Equi patch file
    let writePhase1EquiData (directory : string) 
                        (filePrefix : string) 
                        (equiData : Phase1EquiData) : CompilerMonad<unit> = 
        if equiData.IsEmpty then
            mreturn ()
        else
            writeNewEquisFile directory filePrefix equiData.Equis
            
    // Write ClassEqui and ValuaEqui patch files
    let writePhase2EquiData (directory : string) 
                                (filePrefix : string) 
                                (equiData : Phase2EquiData) : CompilerMonad<unit> = 
        if equiData.IsEmpty then
            mreturn ()
        else
            compile {
                do! writeNewClassEquisFile directory filePrefix equiData.ClassEquis
                do! writeNewValuaEquisFile directory filePrefix equiData.ValuaEquis
                return ()
            }

    