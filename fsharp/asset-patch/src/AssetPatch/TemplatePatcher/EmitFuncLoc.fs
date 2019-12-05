// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module EmitFuncLoc =

    open AssetPatch.Base
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.Hierarchy
    open AssetPatch.TemplatePatcher.PatchWriter
    
    type FlocProperties = 
        { ClassFlocs : ClassFloc list
          ValuaFlocs : ValuaFloc list
        }

    let collectFlocProperties (source : FlocProperties list) : FlocProperties = 
        let add (r1 : FlocProperties) (acc : FlocProperties) = 
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

    let funclocToFlocProperties (flocPath : FuncLocPath) 
                                    (classes : S4Class list) : CompilerMonad<FlocProperties> = 
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
                            (filePrefix : string) 
                            (flocProperties : FlocProperties) : CompilerMonad<unit> = 
        compile {
            do! writeClassFlocFile directory filePrefix flocProperties.ClassFlocs
            do! writeValuaFlocFile directory filePrefix flocProperties.ValuaFlocs
            return ()
        }

    let writeFlocResults (directory : string) 
                            (filePrefix : string) 
                            (funcLocResults : FuncLocResults) : CompilerMonad<unit> = 
        compile {
            do! writeFuncLocFile directory filePrefix funcLocResults.FuncLocs
            do! writeClassFlocFile directory filePrefix funcLocResults.ClassFlocs
            do! writeValuaFlocFile directory filePrefix funcLocResults.ValuaFlocs
            return ()
        }
