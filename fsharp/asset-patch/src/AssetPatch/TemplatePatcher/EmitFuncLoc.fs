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
    open AssetPatch.TemplatePatcher.EmitCommon

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

