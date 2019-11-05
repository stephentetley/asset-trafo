﻿// Copyright (c) Stephen Tetley 2019
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

  

    type EmitterResults = 
        { Equis : Equi list
          ClassEquis : ClassEqui list
          ValuaEquis : ValuaEqui list
          FuncLocs : FuncLoc list
          ClassFlocs : ClassFloc list
          ValuaFlocs : ValuaFloc list
        }

        static member Empty = 
            { Equis = []
              ClassEquis = []
              ValuaEquis = []
              FuncLocs = []
              ClassFlocs = []
              ValuaFlocs = []
            }


    let collectEquiClassProperties (source:  EquiClassProperties list) :  ClassEqui list * ValuaEqui list =
        List.foldBack (fun (ce,vs) (cs, valuas) -> (ce :: cs, valuas @ vs)) source ([],[])

    let collectFlocClassProperties (source:  FlocClassProperties list) :  ClassFloc list * ValuaFloc list =
        List.foldBack (fun (ce,vs) (cs, valuas) -> (ce :: cs, valuas @ vs)) source ([],[])

    let joinResults (a : EmitterResults) (b: EmitterResults) : EmitterResults = 
        { Equis = a.Equis@ b.Equis
          ClassEquis = a.ClassEquis @ b.ClassEquis
          ValuaEquis = a.ValuaEquis @ b.ValuaEquis
          FuncLocs = a.FuncLocs @ b.FuncLocs
          ClassFlocs = a.ClassFlocs @ b.ClassFlocs
          ValuaFlocs = a.ValuaFlocs @ b.ValuaFlocs
        }

    let concatResults ( source : EmitterResults list) : EmitterResults = 
        List.foldBack joinResults  source EmitterResults.Empty



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
    
    let equipmentClassProperties1 (equipment: Equipment) : CompilerMonad<EquiClassProperties list, 'env> = 
        match equipment.MagicNumber with
        | None -> throwError "Emitter - equipment has not been renamed"
        | Some magic -> 
            forM equipment.Classes (makeEquiProperties1 (EquipmentMagic magic))


    let equipmentClassProperties (equipment: Equipment) : CompilerMonad<EquiClassProperties list, 'env> = 
        let rec work (kids : Equipment list) 
                     (cont : EquiClassProperties list -> CompilerMonad<EquiClassProperties list, 'env>) = 
            match kids with
            | [] -> cont []
            | x :: rest -> 
                compile { 
                    let! vs1 = equipmentClassProperties1 x
                    return! work x.SuboridnateEquipment (fun vs2 -> 
                            work rest (fun vs3 -> 
                            cont (vs1 @ vs2 @ vs3)))
                } 
        compile { 
            let! es1 = equipmentClassProperties1 equipment
            return! work equipment.SuboridnateEquipment (fun es -> mreturn (es1 @ es))
        }


    let equipmentEmit (parent : FuncLocPath) 
                      (equipment: Equipment) : CompilerMonad<EmitterResults, 'env> = 
        compile {
            let! es = equipmentToEquis parent equipment
            let! cps = equipmentClassProperties equipment
            let (cs,vs) = collectEquiClassProperties cps
            return {
                Equis = es
                ClassEquis = cs
                ValuaEquis = vs
                FuncLocs = []
                ClassFlocs = []
                ValuaFlocs = []
            }
        }

    let funcLocClassProperties (path : FuncLocPath) 
                                (classes: Class list) : CompilerMonad<FlocClassProperties list, 'env> = 
        mapM (makeFlocProperties1 path) classes

    let funcLocEmit (funcLoc : FuncLoc) 
                    (classes: Class list) : CompilerMonad<EmitterResults, 'env> = 
        compile {
            let! cps = funcLocClassProperties funcLoc.Path classes
            let (cs, vs) = collectFlocClassProperties cps
            return { 
                Equis = []
                ClassEquis = []
                ValuaEquis = []
                FuncLocs = [funcLoc]
                ClassFlocs = cs
                ValuaFlocs = vs
            }
        }

    let componentEmit (parent : FuncLocPath) 
                      (compo : Component) : CompilerMonad<EmitterResults, 'env> = 
        let path = extend compo.FuncLocSegment.Name parent
        let funcLoc = 
            { Path = path
              Description = compo.FuncLocSegment.Description
              ObjectType = compo.FuncLocSegment.ObjectType
              Attributes = AssocList.empty
            }
        compile {
            let! ansE = mapM (equipmentEmit path) compo.Equipment |>> concatResults
            let! ansF = funcLocEmit funcLoc compo.Classes
            return joinResults ansF ansE
        }

    let itemEmit (parent : FuncLocPath) 
                 (item : Item) : CompilerMonad<EmitterResults, 'env> = 
        let path = extend item.FuncLocSegment.Name parent
        let funcLoc = 
            { Path = path
              Description = item.FuncLocSegment.Description
              ObjectType = item.FuncLocSegment.ObjectType
              Attributes = AssocList.empty
            }
        compile {
            let! ansE = mapM (equipmentEmit path) item.Equipment |>> concatResults
            let! ansF = funcLocEmit funcLoc item.Classes
            let! ansK = mapM (componentEmit path) item.Components |>> concatResults
            return concatResults [ansF; ansE; ansK]
        }

    let assemblyEmit (parent : FuncLocPath) 
                     (assembly : Assembly) : CompilerMonad<EmitterResults, 'env> = 
        let path = extend assembly.FuncLocSegment.Name parent
        let funcLoc = 
            { Path = path
              Description = assembly.FuncLocSegment.Description
              ObjectType = assembly.FuncLocSegment.ObjectType
              Attributes = AssocList.empty
            }
        compile {
            let! ansE = mapM (equipmentEmit path) assembly.Equipment |>> concatResults
            let! ansF = funcLocEmit funcLoc assembly.Classes
            let! ansK = mapM (itemEmit path) assembly.Items |>> concatResults
            return concatResults [ansF; ansE; ansK]
        }

    let systemEmit (parent : FuncLocPath) 
                     (system : System) : CompilerMonad<EmitterResults, 'env> = 
        let path = extend system.FuncLocSegment.Name parent
        let funcLoc = 
            { Path = path
              Description = system.FuncLocSegment.Description
              ObjectType = system.FuncLocSegment.ObjectType
              Attributes = AssocList.empty
            }
        compile {
            let! ansE = mapM (equipmentEmit path) system.Equipment |>> concatResults
            let! ansF = funcLocEmit funcLoc system.Classes
            let! ansK = mapM (assemblyEmit path) system.Assemblies |>> concatResults
            return concatResults [ansF; ansE; ansK]
        }

    let processEmit (parent : FuncLocPath) 
                    (proc : Process) : CompilerMonad<EmitterResults, 'env> = 
        let path = extend proc.FuncLocSegment.Name parent
        let funcLoc = 
            { Path = path
              Description = proc.FuncLocSegment.Description
              ObjectType = proc.FuncLocSegment.ObjectType
              Attributes = AssocList.empty
            }
        compile {
            let! ansF = funcLocEmit funcLoc proc.Classes
            let! ansK = mapM (systemEmit path) proc.Systems |>> concatResults
            return joinResults ansF ansK
        }

    let processGroupEmit (parent : FuncLocPath) 
                            (procGroup : ProcessGroup) : CompilerMonad<EmitterResults, 'env> = 
        let path = extend procGroup.FuncLocSegment.Name parent
        let funcLoc = 
            { Path = path
              Description = procGroup.FuncLocSegment.Description
              ObjectType = procGroup.FuncLocSegment.ObjectType
              Attributes = AssocList.empty
            }
        compile {
            let! ansF = funcLocEmit funcLoc procGroup.Classes
            let! ansK = mapM (processEmit path) procGroup.Processes |>> concatResults
            return joinResults ansF ansK
        }

    let functionEmit (parent : FuncLocPath) 
                        (func : Function) : CompilerMonad<EmitterResults, 'env> = 
        let path = extend func.FuncLocSegment.Name parent
        let funcLoc = 
            { Path = path
              Description = func.FuncLocSegment.Description
              ObjectType = func.FuncLocSegment.ObjectType
              Attributes = AssocList.empty
            }
        compile {
            let! ansF = funcLocEmit funcLoc func.Classes
            let! ansK = mapM (processGroupEmit path) func.ProcessGroups |>> concatResults
            return joinResults ansF ansK
        }

    let siteEmit (site : Site) : CompilerMonad<EmitterResults, 'env> = 
        let path = FuncLocPath.Create site.FuncLocSegment.Name
        let funcLoc = 
            { Path = path
              Description = site.FuncLocSegment.Description
              ObjectType = site.FuncLocSegment.ObjectType
              Attributes = AssocList.empty
            }
        compile {
            let! ansF = funcLocEmit funcLoc site.Classes
            let! ansK = mapM (functionEmit path) site.Functions |>> concatResults
            return joinResults ansF ansK
        }