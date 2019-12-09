// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module EmitFuncLoc =

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.Hierarchy
    open AssetPatch.TemplatePatcher.PatchWriter
    
    type ClassFlocInstances = 
        { ClassFlocs : ClassFloc list
          ValuaFlocs : ValuaFloc list
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
        { FuncLoc : FuncLoc
          ClassFlocs : ClassFloc list
          ValuaFlocs : ValuaFloc list
        }

    type FuncLocResults = 
        { FuncLocs : FuncLoc list
          ClassFlocs : ClassFloc list
          ValuaFlocs : ValuaFloc list
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
                                    (count : int) 
                                    (charac : S4Characteristic) : CompilerMonad<ValuaFloc> = 
        mreturn {   
            FuncLoc = funcLoc
            ClassType = IntegerString.OfString "003"
            CharacteristicID = charac.Name
            CharacteristicValue = charac.Value
            ValueCount = count
        }

    

    let classToClassFloc (funcLoc : FuncLocPath)  (clazz : S4Class) : CompilerMonad<ClassFloc> = 
        mreturn { 
            FuncLoc = funcLoc
            Class = clazz.ClassName
            ClassType = IntegerString.OfString "003"
            ClassNumber = IntegerString.Create(10, clazz.ClassInt)
            Status = 1
        }




    let translateS4CharacteristicsFloc (flocPath : FuncLocPath)
                                        (characteristics : S4Characteristic list) : CompilerMonad<ValuaFloc list> =  

        let makeGrouped (chars : S4Characteristic list) : CompilerMonad<ValuaFloc list> = 
            foriM chars (fun i x -> characteristicToValuaFloc flocPath (i+1) x)

        compile {
            let chars = sortedCharacteristics characteristics
            return! mapM makeGrouped chars |>> List.concat
        }

  
    let translateS4ClassFloc (flocPath : FuncLocPath)
                                (clazz : S4Class) : CompilerMonad<ClassFloc * ValuaFloc list> = 
        compile {
            let! ce = classToClassFloc flocPath clazz
            let! vs = translateS4CharacteristicsFloc flocPath clazz.Characteristics
            return (ce, vs)
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
            }
        }

    let funclocToFuncLocResult1 (path : FuncLocPath) 
                                (description : string) 
                                (objectType : string)
                                (classes : S4Class list) : CompilerMonad<FuncLocResult1> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile {
            let! floc = genFuncLoc path description objectType
            let! (cs, vs) = mapM (translateS4ClassFloc path) classes |>> collect
            return { 
                FuncLoc = floc
                ClassFlocs = cs
                ValuaFlocs = vs
            }
        }

    let funclocToClassFlocInstances (flocPath : FuncLocPath) 
                                    (classes : S4Class list) : CompilerMonad<ClassFlocInstances> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile { 
            let! (cs, vs) = mapM (translateS4ClassFloc flocPath) classes |>> collect
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
                do! writeFuncLocFile directory level filePrefix funcLocResults.FuncLocs
                do! writeClassFlocFile directory level filePrefix funcLocResults.ClassFlocs
                do! writeValuaFlocFile directory level filePrefix funcLocResults.ValuaFlocs
                return ()
            }
