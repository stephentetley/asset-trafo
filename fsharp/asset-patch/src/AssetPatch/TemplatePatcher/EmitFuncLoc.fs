// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module EmitFuncLoc =

    open System.IO

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.EquiIndexing
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.PatchWriter
    
    type ClassFlocInstances = 
        { ClassFlocs : CreateClassFloc list
          ValuaFlocs : CreateValuaFloc list
        }
        member x.IsEmpty 
            with get () : bool = 
                x.ClassFlocs.IsEmpty && x.ValuaFlocs.IsEmpty

    let collectClassFlocInstances (source : ClassFlocInstances list) : ClassFlocInstances = 
        let add (r1 : ClassFlocInstances) (acc : ClassFlocInstances) = 
            { ClassFlocs = r1.ClassFlocs @ acc.ClassFlocs
              ValuaFlocs = r1.ValuaFlocs @ acc.ValuaFlocs
            }
        List.foldBack add source { ClassFlocs = []; ValuaFlocs = [] }

    type FuncLocResult1 = 
        { FuncLoc : CreateFuncLoc
          ClassFlocs : CreateClassFloc list
          ValuaFlocs : CreateValuaFloc list
        }

    type FuncLocResults = 
        { FuncLocs : CreateFuncLoc list
          ClassFlocs : CreateClassFloc list
          ValuaFlocs : CreateValuaFloc list
        }
        member x.IsEmpty 
            with get () : bool = 
                x.FuncLocs.IsEmpty &&  x.ClassFlocs.IsEmpty && x.ValuaFlocs.IsEmpty

    let collectFuncLocResults (source : FuncLocResult1 list) : FuncLocResults = 
        let add (r1 : FuncLocResult1) (acc : FuncLocResults) = 
            { FuncLocs = r1.FuncLoc :: acc.FuncLocs
              ClassFlocs = r1.ClassFlocs @ acc.ClassFlocs
              ValuaFlocs = r1.ValuaFlocs @ acc.ValuaFlocs
            }
        List.foldBack add source { FuncLocs = []; ClassFlocs = []; ValuaFlocs = [] }


    let characteristicToValuaFloc (funcLoc : FuncLocPath)
                                    (interimId : string option)
                                    (count : int) 
                                    (charac : S4Characteristic) : CompilerMonad<CreateValuaFloc> = 
        mreturn {   
            FuncLoc = funcLoc
            InterimId = interimId
            ClassType = IntegerString.OfString "003"
            CharacteristicID = charac.Name
            ValueCount = count
            Value = charac.Value
        }

    

    let classToClassFloc (funcLoc : FuncLocPath)  
                            (interimId : string option) 
                            (clazz : S4Class) : CompilerMonad<CreateClassFloc> = 
        mreturn { 
            FuncLoc = funcLoc
            InterimId = interimId
            Class = clazz.ClassName
            Status = 1
        }




    let translateS4CharacteristicsFloc (flocPath : FuncLocPath)
                                        (interimId : string option)
                                        (characteristics : S4Characteristic list) : CompilerMonad<CreateValuaFloc list> =  

        let makeGrouped (chars : S4Characteristic list) : CompilerMonad<CreateValuaFloc list> = 
            foriM chars (fun i x -> characteristicToValuaFloc flocPath interimId (i+1) x)

        compile {
            let chars = sortedCharacteristics characteristics
            return! mapM makeGrouped chars |>> List.concat
        }

  
    let translateS4ClassFloc (flocPath : FuncLocPath)
                                (interimId : string option)
                                (clazz : S4Class) : CompilerMonad<CreateClassFloc * CreateValuaFloc list> = 
        compile {
            let! ce = classToClassFloc flocPath interimId clazz
            let! vs = translateS4CharacteristicsFloc flocPath interimId clazz.Characteristics
            return (ce, vs)
        }




    let genFuncLoc (path : FuncLocPath) 
                    (props : FuncLocProperties)
                    (interimId : string option)
                    (description : string) 
                    (objectType : string)  : CompilerMonad<CreateFuncLoc> = 
        let commonProps : CommonProperties = 
            { ControllingArea = props.ControllingArea
              CompanyCode = props.CompanyCode
              PlantCode = props.MaintenancePlant
              UserStatus = props.ObjectStatus }

        compile {
            return { 
                Path = path
                InterimId = interimId
                Description = description
                ObjectType = objectType
                Category = uint32 path.Level
                ObjectStatus = props.ObjectStatus
                StartupDate = props.StartupDate
                StructureIndicator = props.StructureIndicator
                CommonProps = commonProps
            }
        }

    let funclocToFuncLocResult1 (path : FuncLocPath) 
                                (interimId : string option)
                                (props : FuncLocProperties)
                                (description : string) 
                                (objectType : string)
                                (classes : S4Class list) : CompilerMonad<FuncLocResult1> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile {
            let! floc = genFuncLoc path props interimId description objectType
            let! (cs, vs) = mapM (translateS4ClassFloc path interimId) classes |>> collect
            return { 
                FuncLoc = floc
                ClassFlocs = cs
                ValuaFlocs = vs
            }
        }

    let funclocToClassFlocInstances (flocPath : FuncLocPath) 
                                    (interimId : string option)
                                    (classes : S4Class list) : CompilerMonad<ClassFlocInstances> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile { 
            let! (cs, vs) = mapM (translateS4ClassFloc flocPath interimId) classes |>> collect
            return { 
                ClassFlocs = cs
                ValuaFlocs = vs
            }
        }

    // ************************************************************************
    // Write output

    let writeFlocProperties (directory : string) 
                            (level : int)
                            (filePrefix : string) 
                            (flocInstances : ClassFlocInstances) : CompilerMonad<unit> = 
        if flocInstances.IsEmpty then
            mreturn ()
        else
            compile {
                do! writeClassFlocFile directory level filePrefix flocInstances.ClassFlocs
                do! writeValuaFlocFile directory level filePrefix flocInstances.ValuaFlocs
                return ()
            }

    let writeFlocResults (directory : string) 
                            (level : int)
                            (filePrefix : string) 
                            (funcLocResults : FuncLocResults) : CompilerMonad<unit> = 
        
        if funcLocResults.IsEmpty then
            mreturn ()
        else
            compile {
                let! useInterimIds = generateWithInterimIds ()
                let! dirName = genSubFolder directory level
                let indexFile = Path.Combine(dirName, "FlocIndexing.xlsx")
                do! if useInterimIds then writeFlocIndexingSheet indexFile funcLocResults.FuncLocs else mreturn ()
                do! writeFuncLocFile directory level filePrefix funcLocResults.FuncLocs
                do! writeClassFlocFile directory level filePrefix funcLocResults.ClassFlocs
                do! writeValuaFlocFile directory level filePrefix funcLocResults.ValuaFlocs
                return ()
            }
