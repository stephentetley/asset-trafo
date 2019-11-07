// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module Emitter =

    open AssetPatch.Base
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.Hierarchy
    
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
                                    (charac : S4Characteristic) : CompilerMonad<ValuaFloc> = 
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
                                    (charac : S4Characteristic) : CompilerMonad<ValuaEqui> = 
        mreturn { 
            EquipmentNumber = equiNumber
            ClassType = IntegerString.OfString "002"
            CharacteristicID = charac.Name
            CharacteristicValue = charac.Value
            ValueCount = count
            Attributes = AssocList.empty
        }
    

    let classToClassFloc (funcLoc : FuncLocPath)  (clazz : S4Class) : CompilerMonad<ClassFloc> = 
        mreturn { 
            FuncLoc = funcLoc
            Class = clazz.ClassName
            ClassType = IntegerString.OfString "003"
            ClassNumber = IntegerString.Create(10, clazz.ClassInt)
            Status = 1
        }


    let classToClassEqui (equiNumber : EquipmentCode)
                         (clazz : S4Class) : CompilerMonad<ClassEqui> = 
        mreturn { 
            EquipmentNumber = equiNumber
            Class = clazz.ClassName
            ClassType = IntegerString.OfString "002"
            ClassNumber = IntegerString.Create(10, clazz.ClassInt)
            Status = 1
        }
    
    let equipmentToEqui1 (funcLoc : FuncLocPath) 
                         (equipment : S4Equipment) : CompilerMonad<Equi> = 
        compile {
            let! number = newEquipmentName ()
            let! sdate = asks (fun x -> x.StartupDate)
            let! mplant = asks (fun x -> x.MaintenancePlant)
            return { 
                EquipmentNumber = EquipmentCode number
                Description = equipment.Description
                FuncLoc = funcLoc
                ObjectType = equipment.ObjectType
                Manufacturer = Option.defaultValue "TO BE DETERMINED" equipment.Manufacturer
                Model = Option.defaultValue "TO BE DETERMINED" equipment.Model
                StartupDate = sdate
                MaintenancePlant = mplant
                Attributes = AssocList.empty
            }
        }
        

    let equipmentToEquis (funcLoc : FuncLocPath) 
                         (equipment : S4Equipment) : CompilerMonad<Equi list> = 
        let rec work (kids : S4Equipment list) 
                     (cont : Equi list -> CompilerMonad<Equi list>) = 
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

    


    



    let sortedCharacteristics (clazz : S4Class) : (S4Characteristic list) list= 
        clazz.Characteritics 
            |> List.sortBy (fun x -> x.Name)
            |> List.groupBy (fun x -> x.Name)               
            |> List.map snd


    let makeFlocProperties1 (funcLoc : FuncLocPath)  
                            (clazz : S4Class) : CompilerMonad<FlocClassProperties> =
        
        let makeGrouped (chars : S4Characteristic list) : CompilerMonad<ValuaFloc list> = 
            foriM chars (fun i x -> characteristicToValuaFloc funcLoc (i+1) x)

        compile {
            let! cf = classToClassFloc funcLoc clazz
            let chars = sortedCharacteristics clazz
            let! vs = mapM makeGrouped chars |>> List.concat
            return (cf, vs)
        } 

    let makeEquiProperties1 (equiNumber : EquipmentCode)  
                            (clazz : S4Class) : CompilerMonad<EquiClassProperties> =
        
        let makeGrouped (chars : S4Characteristic list) : CompilerMonad<ValuaEqui list> = 
            foriM chars (fun i x -> characteristicToValuaEqui equiNumber (i+1) x)

        compile {
            let! ce = classToClassEqui equiNumber clazz
            let chars = sortedCharacteristics clazz
            let! vs = mapM makeGrouped chars |>> List.concat
            return (ce, vs)
        } 
    
    let equipmentClassProperties1 (equipment : S4Equipment) : CompilerMonad<EquiClassProperties list> = 
        match equipment.EquipmentId with
        | None -> throwError "Emitter - equipment has not been renamed"
        | Some magic -> 
            forM equipment.Classes (makeEquiProperties1 (EquipmentCode magic))


    let equipmentClassProperties (equipment : S4Equipment) : CompilerMonad<EquiClassProperties list> = 
        let rec work (kids : S4Equipment list) 
                     (cont : EquiClassProperties list -> CompilerMonad<EquiClassProperties list>) = 
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


    let equipmentEmitClassValuas (equipment : S4Equipment) : CompilerMonad<EmitterResults> = 
        compile {
            let! cps = equipmentClassProperties equipment
            let (cs,vs) = collectEquiClassProperties cps
            return {
                Equis = []
                ClassEquis = cs
                ValuaEquis = vs
                FuncLocs = []
                ClassFlocs = []
                ValuaFlocs = []
            }
        }


    let equipmentEmit (parent : FuncLocPath) 
                      (equipment : S4Equipment) : CompilerMonad<EmitterResults> = 
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
                                (classes : S4Class list) : CompilerMonad<FlocClassProperties list> = 
        mapM (makeFlocProperties1 path) classes




    let funcLocPathEmitClassValuas (funcLocPath : FuncLocPath) 
                                    (classes : S4Class list) : CompilerMonad<EmitterResults> = 
        compile {
            let! cps = funcLocClassProperties funcLocPath classes
            let (cs, vs) = collectFlocClassProperties cps
            return { 
                Equis = []
                ClassEquis = []
                ValuaEquis = []
                FuncLocs = []
                ClassFlocs = cs
                ValuaFlocs = vs
            }
        }


    let funcLocEmit (funcLoc : FuncLoc) 
                    (classes : S4Class list) : CompilerMonad<EmitterResults> = 
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


    let genFuncLoc (path : FuncLocPath) 
                    (description : string) 
                    (objectType : string)  : CompilerMonad<FuncLoc> = 
        compile {
            let! startDate = asks (fun x -> x.StartupDate) 
            let! structInd = asks (fun x -> x.StructureIndicator)
            let! objStatus = asks (fun x -> x.ObjectStatus)
            return { 
                Path = path
                Description = description
                ObjectType = objectType
                Category = uint32 path.Level
                ObjectStatus = objStatus
                StartupDate = startDate
                StructureIndicator = structInd
                Attributes = AssocList.empty
            }
        }
    let componentEmit (parent : FuncLocPath) 
                      (compo : S4Component) : CompilerMonad<EmitterResults> = 
        let path = extend compo.FuncLocSegment.Name parent
        compile {
            let! floc = genFuncLoc path compo.FuncLocSegment.Description compo.FuncLocSegment.ObjectType
            let! ansE = mapM (equipmentEmit path) compo.Equipment |>> concatResults
            let! ansF = funcLocEmit floc compo.Classes
            return joinResults ansF ansE
        }

    let itemEmit (parent : FuncLocPath) 
                 (item : S4Item) : CompilerMonad<EmitterResults> = 
        let path = extend item.FuncLocSegment.Name parent
        compile {
            let! floc = genFuncLoc path item.FuncLocSegment.Description item.FuncLocSegment.ObjectType
            let! ansE = mapM (equipmentEmit path) item.Equipment |>> concatResults
            let! ansF = funcLocEmit floc item.Classes
            let! ansK = mapM (componentEmit path) item.Components |>> concatResults
            return concatResults [ansF; ansE; ansK]
        }

    let assemblyEmit (parent : FuncLocPath) 
                     (assembly : S4Assembly) : CompilerMonad<EmitterResults> = 
        let path = extend assembly.FuncLocSegment.Name parent
        compile {
            let! floc = genFuncLoc path assembly.FuncLocSegment.Description assembly.FuncLocSegment.ObjectType
            let! ansE = mapM (equipmentEmit path) assembly.Equipment |>> concatResults
            let! ansF = funcLocEmit floc assembly.Classes
            let! ansK = mapM (itemEmit path) assembly.Items |>> concatResults
            return concatResults [ansF; ansE; ansK]
        }

    let systemEmit (parent : FuncLocPath) 
                     (system : S4System) : CompilerMonad<EmitterResults> = 
        let path = extend system.FuncLocSegment.Name parent
        compile {
            let! floc = genFuncLoc path system.FuncLocSegment.Description system.FuncLocSegment.ObjectType
            let! ansE = mapM (equipmentEmit path) system.Equipment |>> concatResults
            let! ansF = funcLocEmit floc system.Classes
            let! ansK = mapM (assemblyEmit path) system.Assemblies |>> concatResults
            return concatResults [ansF; ansE; ansK]
        }

    let processEmit (parent : FuncLocPath) 
                    (proc : S4Process) : CompilerMonad<EmitterResults> = 
        let path = extend proc.FuncLocSegment.Name parent
        compile {
            let! floc = genFuncLoc path proc.FuncLocSegment.Description proc.FuncLocSegment.ObjectType
            let! ansF = funcLocEmit floc proc.Classes
            let! ansK = mapM (systemEmit path) proc.Systems |>> concatResults
            return joinResults ansF ansK
        }

    let processGroupEmit (parent : FuncLocPath) 
                            (procGroup : S4ProcessGroup) : CompilerMonad<EmitterResults> = 
        let path = extend procGroup.FuncLocSegment.Name parent
        compile {
            let! floc = genFuncLoc path procGroup.FuncLocSegment.Description procGroup.FuncLocSegment.ObjectType
            let! ansF = funcLocEmit floc procGroup.Classes
            let! ansK = mapM (processEmit path) procGroup.Processes |>> concatResults
            return joinResults ansF ansK
        }

    let functionEmit (parent : FuncLocPath) 
                        (func : S4Function) : CompilerMonad<EmitterResults> = 
        let path = extend func.FuncLocSegment.Name parent
        compile {
            let! floc = genFuncLoc path func.FuncLocSegment.Description func.FuncLocSegment.ObjectType
            let! ansF = funcLocEmit floc func.Classes
            let! ansK = mapM (processGroupEmit path) func.ProcessGroups |>> concatResults
            return joinResults ansF ansK
        }

    let siteEmit (site : S4Site) : CompilerMonad<EmitterResults> = 
        let path = FuncLocPath.Create site.FuncLocSegment.Name
        compile {
            let! floc = genFuncLoc path site.FuncLocSegment.Description site.FuncLocSegment.ObjectType
            let! ansF = funcLocEmit floc site.Classes
            let! ansK = mapM (functionEmit path) site.Functions |>> concatResults
            return joinResults ansF ansK
        }