// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module EmitFuncLoc =

    open System.IO

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.PatchWriter
    
    type NewFlocProperties = 
        { ClassFlocs : NewClassFloc list
          ValuaFlocs : NewValuaFloc list
        }
        member x.IsEmpty 
            with get () : bool = 
                x.ClassFlocs.IsEmpty && x.ValuaFlocs.IsEmpty

    let private concatNewFlocProperties (source : NewFlocProperties list) : NewFlocProperties = 
        let add (r1 : NewFlocProperties) (acc : NewFlocProperties) = 
            { ClassFlocs = r1.ClassFlocs @ acc.ClassFlocs
              ValuaFlocs = r1.ValuaFlocs @ acc.ValuaFlocs
            }
        List.foldBack add source { ClassFlocs = []; ValuaFlocs = [] }

    type FuncLocResult1 = 
        { FuncLoc : NewFuncLoc
          FuncLocLink : LinkFuncLoc option
          ClassFlocs : NewClassFloc list
          ValuaFlocs : NewValuaFloc list
        }

    type Phase1FlocData = 
        { FuncLocs : NewFuncLoc list
          FuncLocLinks : LinkFuncLoc list
          ClassFlocs : NewClassFloc list
          ValuaFlocs : NewValuaFloc list
        }
        member x.IsEmpty 
            with get () : bool = 
                x.FuncLocs.IsEmpty &&  x.ClassFlocs.IsEmpty && x.ValuaFlocs.IsEmpty

        member x.RemoveDups() : Phase1FlocData = 
            { FuncLocs = x.FuncLocs |> List.distinctBy (fun x -> x.FunctionLocation)
              FuncLocLinks = x.FuncLocLinks |> List.distinctBy (fun x -> x.FunctionLocation)
              ClassFlocs = x.ClassFlocs |> List.distinctBy (fun x -> x.FuncLoc.ToString() + "::" + x.Class)
              ValuaFlocs = x.ValuaFlocs |> List.distinct
            }

    let concatPhase1FlocData (source : Phase1FlocData list) : Phase1FlocData = 
        { FuncLocs = source |> List.map (fun x -> x.FuncLocs) |> List.concat
          FuncLocLinks = source |> List.map (fun x -> x.FuncLocLinks) |> List.concat
          ClassFlocs = source |> List.map (fun x -> x.ClassFlocs) |> List.concat
          ValuaFlocs = source |> List.map (fun x -> x.ValuaFlocs) |> List.concat
        }

    let concatFuncLocResult1s (source : FuncLocResult1 list) : Phase1FlocData = 
        let add (r1 : FuncLocResult1) (acc : Phase1FlocData) = 
            { FuncLocs = r1.FuncLoc :: acc.FuncLocs
              FuncLocLinks = 
                match r1.FuncLocLink with
                | None -> acc.FuncLocLinks
                | Some link -> link :: acc.FuncLocLinks
              ClassFlocs = r1.ClassFlocs @ acc.ClassFlocs
              ValuaFlocs = r1.ValuaFlocs @ acc.ValuaFlocs
            }
        List.foldBack add source { FuncLocs = []; FuncLocLinks = []; ClassFlocs = []; ValuaFlocs = [] }


    let characteristicToNewValuaFloc (funcLoc : FuncLocPath)
                                    (count : int) 
                                    (charac : S4Characteristic) : CompilerMonad<NewValuaFloc> = 
        mreturn {   
            FuncLoc = funcLoc
            ClassType = IntegerString.OfString "003"
            CharacteristicID = charac.Name
            ValueCount = count
            Value = charac.Value
        }

    

    let classToNewClassFloc (funcLoc : FuncLocPath)  
                            (clazz : S4Class) : CompilerMonad<NewClassFloc> = 
        mreturn { 
            FuncLoc = funcLoc
            Class = clazz.ClassName
            Status = 1
        }


    let private characteristicsToNewValuaFlocs (flocPath : FuncLocPath)
                                        (characteristics : S4Characteristic list) : CompilerMonad<NewValuaFloc list> =  

        let makeGrouped (chars : S4Characteristic list) : CompilerMonad<NewValuaFloc list> = 
            foriM chars (fun i x -> characteristicToNewValuaFloc flocPath (i+1) x)

        compile {
            let chars = sortedCharacteristics characteristics
            return! mapM makeGrouped chars |>> List.concat
        }

  
    let private classToProperties  (flocPath : FuncLocPath)
                                    (clazz : S4Class) : CompilerMonad<NewClassFloc * NewValuaFloc list> = 
        compile {
            let! ce = classToNewClassFloc flocPath clazz
            let! vs = characteristicsToNewValuaFlocs flocPath clazz.Characteristics
            return (ce, vs)
        }




    let genNewFuncLoc (path : FuncLocPath) 
                    (props : FuncLocProperties)
                    (description : string) 
                    (objectType : string)  : CompilerMonad<NewFuncLoc> = 
        let commonProps : CommonProperties = 
            { ControllingArea = props.ControllingArea
              CompanyCode = props.CompanyCode
              PlantCode = props.MaintenancePlant
              UserStatus = props.ObjectStatus }

        compile {
            return { 
                FunctionLocation = path
                Description = description
                ObjectType = objectType
                Category = uint32 path.Level
                ObjectStatus = props.ObjectStatus
                StartupDate = props.StartupDate
                StructureIndicator = props.StructureIndicator
                CommonProps = commonProps
            }
        }

    let genFuncLocLink (path : FuncLocPath) 
                    (props : FuncLocProperties)
                    (description : string) 
                    (objectType : string)  : CompilerMonad<LinkFuncLoc option> = 
        if path.Level > 1 then
            let link = { 
                    FunctionLocation = path
                    Description = description
                    ObjectType = objectType
                    Category = uint32 path.Level
                    ObjectStatus = props.ObjectStatus
                    StartupDate = props.StartupDate
                    StructureIndicator = props.StructureIndicator
                }
            mreturn (Some link)
        else mreturn None

    let funclocToFuncLocResult1 (path : FuncLocPath) 
                                (props : FuncLocProperties)
                                (description : string) 
                                (objectType : string)
                                (classes : S4Class list) : CompilerMonad<FuncLocResult1> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile {
            let! floc = genNewFuncLoc path props description objectType
            let! link = genFuncLocLink path props description objectType
            let! (cs, vs) = mapM (classToProperties path) classes |>> collect
            return { 
                FuncLoc = floc
                FuncLocLink = link
                ClassFlocs = cs
                ValuaFlocs = vs
            }
        }

    let funclocToNewFlocProperties (flocPath : FuncLocPath) 
                                    (classes : S4Class list) : CompilerMonad<NewFlocProperties> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile { 
            let! (cs, vs) = mapM (classToProperties flocPath) classes |>> collect
            return { 
                ClassFlocs = cs
                ValuaFlocs = vs
            }
        }

    // ************************************************************************
    // Write output


    let writePhase1FlocData (directory : string) 
                            (filePrefix : string) 
                            (funcLocResults : Phase1FlocData) : CompilerMonad<unit> = 
        
        if funcLocResults.IsEmpty then
            mreturn ()
        else
            compile {
                do! writeNewFuncLocsFile directory filePrefix funcLocResults.FuncLocs
                do! writeLinkFuncLocsFile directory filePrefix funcLocResults.FuncLocLinks
                do! writeNewClassFlocsFile directory filePrefix funcLocResults.ClassFlocs
                do! writeNewValuaFlocsFile directory filePrefix funcLocResults.ValuaFlocs
                return ()
            }

