// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base



module EntityTypes =
    
    open AssetPatch.Base.Common
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.AbsChangeFile
    open AssetPatch.Base.Parser
    open AssetPatch.Base.FuncLocPath
   
    // ************************************************************************
    // FuncLoc

    


    // The other way is to look at differences to an existing root funcloc
    // Then only 8 fields change:
    //
    // 1   FUNCLOC
    // 2   TXTMI
    // 38  FLTYP
    // 42  IEQUI
    // 56  FLOC_REF  {- Magic -}
    // 62  EQART
    // 63  JOBJN_FL  {- Magic -}
    // 94  TPLMA1
    // 95  TPLMA
    

    type FuncLoc = 
      { Path : FuncLocPath
        Description : string
        ObjectType : string
        Attributes : AssocList<string, string>
      }
        member x.Level with get () : int = x.Path.Level

        



    let assocsToFuncLoc (attributes : AssocList<string, string>) : Result<FuncLoc, ErrMsg> = 
        match AssocList.tryFind3 "FUNCLOC" "TXTMI" "EQART" attributes with
        | Some (funcloc, desc, otype) -> 
            Ok { Path = FuncLocPath.Create funcloc 
                 Description = desc
                 ObjectType = otype
                 Attributes = attributes }
         | None -> Error "Could not find required fields for a FuncLoc"

    let funcLocToAssocs (funcLoc: FuncLoc) : AssocList<string, string> = 
        funcLoc.Attributes
            |> AssocList.update "FUNCLOC"   (funcLoc.Path.ToString())
            |> AssocList.update "TXTMI"     funcLoc.Description
            |> AssocList.update "EQART"     funcLoc.ObjectType

    let readFuncLocChangeFile (inputFile : string) : CompilerMonad<FuncLoc list, 'env, 'acc> = 
        compile { 
            let! ast = liftResult (readChangeFile inputFile) |>> ofChangeFile
            return! mapM (liftResult << assocsToFuncLoc) ast.Rows
        }

    type FuncLocSegment = 
        { Name : string
          Description : string
          ObjectType: string 
        }

    let extendFuncLoc (segment : FuncLocSegment) 
                      (floc: FuncLoc) : FuncLoc = 
        { Path = FuncLocPath.extend segment.Name floc.Path
          Description = segment.Description
          ObjectType = segment.Description
          Attributes = floc.Attributes }

    // ************************************************************************
    // ClassFloc


    type ClassFloc = 
      { FuncLoc : FuncLocPath
        Class : string
        ClassType : IntegerString
        ClassNumber : IntegerString
        Status : int
      }

    let assocsToClassFloc (attributes : AssocList<string, string>) : Result<ClassFloc, ErrMsg> = 
        match AssocList.tryFind5 "FUNCLOC" "CLASS" "CLASSTYPE" "CLINT" "CLSTATUS1" attributes with
        | Some (funcloc, claz, claztype, clint, status) -> 
            Ok { FuncLoc = FuncLocPath.Create funcloc 
                 Class = claz
                 ClassType = IntegerString.OfString claztype
                 ClassNumber = IntegerString.OfString clint
                 Status = int status }
         | None -> Error "Could not find required fields for a ClassFloc"

    let classFlocToAssocs (classFloc: ClassFloc) : AssocList<string, string> = 
        AssocList.ofList
            [ ("FUNCLOC",       classFloc.FuncLoc.ToString())
            ; ("CLASS",         classFloc.Class)
            ; ("CLASSTYPE",     classFloc.ClassType.Number)
            ; ("CLINT",         classFloc.ClassNumber.Number)
            ; ("CLSTATUS1",     classFloc.Status.ToString())
            ]

    let readClassFlocChangeFile (inputFile : string) : CompilerMonad<ClassFloc list, 'env, 'acc> = 
        compile { 
            let! ast = liftResult (readChangeFile inputFile) |>> ofChangeFile
            return! mapM (liftResult << assocsToClassFloc) ast.Rows
        }

    // ************************************************************************
    // ValuaFloc

    type ValuaFloc = 
      { FuncLoc : FuncLocPath
        ClassType : IntegerString
        CharacteristicID : string
        CharacteristicValue : string
        Attributes : AssocList<string, string>
      }

    let assocsToValuaFloc (attributes : AssocList<string, string>) : Result<ValuaFloc, ErrMsg> = 
        match AssocList.tryFind4 "FUNCLOC" "CLASSTYPE" "CHARID" "ATWRT" attributes with
        | Some (funcloc, claztype, cid, cvalue) -> 
            Ok { FuncLoc = FuncLocPath.Create funcloc 
                 ClassType = IntegerString.OfString claztype
                 CharacteristicID = cid
                 CharacteristicValue = cvalue
                 Attributes = attributes }
         | None -> Error "Could not find required fields for a ValuaFloc"

    /// Note - CharacteristicValue is used three times.
    let valuaFlocToAssocs (valuaFloc: ValuaFloc) : AssocList<string, string> = 
        valuaFloc.Attributes
            |> AssocList.update "FUNCLOC"       (valuaFloc.FuncLoc.ToString())
            |> AssocList.update "CLASSTYPE"     valuaFloc.ClassType.Number
            |> AssocList.update "CHARID"        valuaFloc.CharacteristicID
            |> AssocList.update "ATWRT"         valuaFloc.CharacteristicValue
            |> AssocList.update "TEXTBEZ"       valuaFloc.CharacteristicValue
            |> AssocList.update "ATFLV"         valuaFloc.CharacteristicValue


    let readValuaFlocChangeFile (inputFile : string) : CompilerMonad<ValuaFloc list, 'env, 'acc> = 
        compile { 
            let! ast = liftResult (readChangeFile inputFile) |>> ofChangeFile
            return! mapM (liftResult << assocsToValuaFloc) ast.Rows
        }

    // ************************************************************************
    // Equi

    type Equi = 
      { EquipmentNumber : IntegerString
        Description : string
        FuncLoc : FuncLocPath
        Attributes : AssocList<string, string>
      }

    let assocsToEqui (attributes : AssocList<string, string>) : Result<Equi, ErrMsg> = 
        match AssocList.tryFind3 "EQUI" "TXTMI" "TPLN_EILO" attributes with
        | Some (number, desc, funcloc) -> 
            Ok { EquipmentNumber = IntegerString.OfString number
                 Description = desc
                 FuncLoc = FuncLocPath.Create funcloc 
                 Attributes = attributes }
         | None -> Error "Could not find required fields for a Equi"

    /// Note - CharacteristicValue is used three times.
    let equiToAssocs (equi: Equi) : AssocList<string, string> = 
        equi.Attributes
            |> AssocList.update "EQUI"          equi.EquipmentNumber.Number
            |> AssocList.update "TXTMI"         equi.Description
            |> AssocList.update "TPLN_EILO"     (equi.FuncLoc.ToString()) 

    let readEquiChangeFile (inputFile : string) : CompilerMonad<Equi list, 'env, 'acc> = 
        compile { 
            let! ast = liftResult (readChangeFile inputFile) |>> ofChangeFile
            return! mapM (liftResult << assocsToEqui) ast.Rows
        }

    // ************************************************************************
    // ClassEqui
    
    type ClassEqui = 
        { EquipmentNumber : IntegerString
          Class : string
          ClassType : IntegerString
          ClassNumber : IntegerString
          Status : int
        }

    let assocsToClassEqui (attributes : AssocList<string, string>) : Result<ClassEqui, ErrMsg> = 
        match AssocList.tryFind5 "EQUI" "CLASS" "CLASSTYPE" "CLINT" "CLSTATUS1" attributes with
        | Some (number, claz, claztype, clint, status) -> 
            Ok { EquipmentNumber = IntegerString.OfString number
                 Class = claz
                 ClassType = IntegerString.OfString claztype
                 ClassNumber = IntegerString.OfString clint
                 Status = int status }
         | None -> Error "Could not find required fields for a ClassEqui"

    let classEquiToAssocs (classEqui: ClassEqui) : AssocList<string, string> = 
        AssocList.ofList
            [ ("EQUI",          classEqui.EquipmentNumber.Number)
            ; ("CLASS",         classEqui.Class)
            ; ("CLASSTYPE",     classEqui.ClassType.Number)
            ; ("CLINT",         classEqui.ClassNumber.Number)
            ; ("CLSTATUS1",     classEqui.Status.ToString())
            ]

    let readClassEquiChangeFile (inputFile : string) : CompilerMonad<ClassEqui list, 'env, 'acc> = 
        compile { 
            let! ast = liftResult (readChangeFile inputFile) |>> ofChangeFile
            return! mapM (liftResult << assocsToClassEqui) ast.Rows
        }

    // ************************************************************************
    // ValuaEqui

    type ValuaEqui = 
        { EquipmentNumber : IntegerString
          ClassType : IntegerString
          CharacteristicID : string
          CharacteristicValue : string
          Attributes : AssocList<string, string>
        }
    
    let assocsToValuaEqui (attributes : AssocList<string, string>) : Result<ValuaEqui, ErrMsg> = 
        match AssocList.tryFind4 "EQUI" "CLASSTYPE" "CHARID" "ATWRT" attributes with
        | Some (number, claztype, cid, cvalue) -> 
            Ok { EquipmentNumber = IntegerString.OfString number
                 ClassType = IntegerString.OfString claztype
                 CharacteristicID = cid
                 CharacteristicValue = cvalue
                 Attributes = attributes }
         | None -> Error "Could not find required fields for a ValuaEqui"

    /// Note - CharacteristicValue is used twice.
    let valuaEquiToAssocs (valuaEqui: ValuaEqui) : AssocList<string, string> = 
        valuaEqui.Attributes
            |> AssocList.update "EQUI"          valuaEqui.EquipmentNumber.Number
            |> AssocList.update "CLASSTYPE"     valuaEqui.ClassType.Number
            |> AssocList.update "CHARID"        valuaEqui.CharacteristicID
            |> AssocList.update "ATWRT"         valuaEqui.CharacteristicValue
            |> AssocList.update "TEXTBEZ"       valuaEqui.CharacteristicValue
            // |> AssocList.update "ATFLV"         valuaEqui.CharacteristicValue

    let readValuaEquiChangeFile (inputFile : string) : CompilerMonad<ValuaEqui list, 'env, 'acc> = 
        compile { 
            let! ast = liftResult (readChangeFile inputFile) |>> ofChangeFile
            return! mapM (liftResult << assocsToValuaEqui) ast.Rows
        }