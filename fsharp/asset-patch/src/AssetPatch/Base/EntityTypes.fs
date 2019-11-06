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

    type FuncLocSegment = 
        { Name : string
          Description : string
          ObjectType: string 
        }




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
    
    /// Note - including uninterpreted attributes is probably not a 
    /// good idea. We should look to finding out exactly what values 
    /// are needed for an upload file.
    type FuncLoc = 
      { Path : FuncLocPath
        Description : string
        ObjectType : string
        Category: uint32
        Attributes : AssocList<string, string>
      }
        member x.Level with get () : int = x.Path.Level

        

    let assocsToFuncLoc (attributes : AssocList<string, string>) : Result<FuncLoc, ErrMsg> = 
        match AssocList.tryFind4 "FUNCLOC" "TXTMI" "FLTYP" "EQART" attributes with
        | Some (funcloc, desc, category, otype) -> 
            Ok { Path = FuncLocPath.Create funcloc 
                 Description = desc
                 ObjectType = otype
                 Category = try (uint32 category) with | _ -> 0u
                 Attributes = attributes }
        | None -> Error "Could not find required fields for a FuncLoc"

    let funcLocToAssocs (funcLoc: FuncLoc) : AssocList<string, string> = 
        funcLoc.Attributes
            |> AssocList.upsert "FUNCLOC"       (funcLoc.Path.ToString())
            |> AssocList.upsert "TXTMI"         funcLoc.Description
            |> AssocList.upsert "EQART"         funcLoc.ObjectType
            |> AssocList.upsert "FLTYP"         (funcLoc.Category.ToString())
            |> AssocList.upsert "TPLKZ_FLC"     "YW-GS"

    let readFuncLocChangeFile (inputFile : string) : CompilerMonad<FuncLoc list, 'env> = 
        compile { 
            let! ast = liftResult (readChangeFile inputFile) |>> ofChangeFile
            return! mapM (liftResult << assocsToFuncLoc) ast.Rows
        }

    

    let extendFuncLoc (segment : FuncLocSegment) 
                      (floc: FuncLoc) : FuncLoc = 
        { Path = FuncLocPath.extend segment.Name floc.Path
          Description = segment.Description
          ObjectType = segment.Description
          Category = floc.Category + 1u
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

    let readClassFlocChangeFile (inputFile : string) : CompilerMonad<ClassFloc list, 'env> = 
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
        ValueCount : int
        Attributes : AssocList<string, string>
      }

    let assocsToValuaFloc (attributes : AssocList<string, string>) : Result<ValuaFloc, ErrMsg> = 
        match AssocList.tryFind5 "FUNCLOC" "CLASSTYPE" "CHARID" "ATWRT" "VALCNT" attributes with
        | Some (funcloc, claztype, cid, cvalue, count) -> 
            Ok { FuncLoc = FuncLocPath.Create funcloc 
                 ClassType = IntegerString.OfString claztype
                 CharacteristicID = cid
                 CharacteristicValue = cvalue
                 ValueCount = int count
                 Attributes = attributes }
         | None -> Error "Could not find required fields for a ValuaFloc"

    /// Note - CharacteristicValue is used three times.
    let valuaFlocToAssocs (valua: ValuaFloc) : AssocList<string, string> = 
        valua.Attributes
            |> AssocList.upsert "FUNCLOC"       (valua.FuncLoc.ToString())
            |> AssocList.upsert "CLASSTYPE"     valua.ClassType.Number
            |> AssocList.upsert "CHARID"        valua.CharacteristicID
            |> AssocList.upsert "ATWRT"         valua.CharacteristicValue
            |> AssocList.upsert "TEXTBEZ"       valua.CharacteristicValue
            |> AssocList.upsert "ATFLV"         valua.CharacteristicValue
            |> AssocList.upsert "VALCNT"        (sprintf "%04i" valua.ValueCount)


    let readValuaFlocChangeFile (inputFile : string) : CompilerMonad<ValuaFloc list, 'env> = 
        compile { 
            let! ast = liftResult (readChangeFile inputFile) |>> ofChangeFile
            return! mapM (liftResult << assocsToValuaFloc) ast.Rows
        }

    // ************************************************************************
    // Equi

    [<Struct>]
    type EquipmentCode = 
        | EquipmentCode of string
    
        member x.Code 
            with get () : string = 
                match x with 
                | EquipmentCode s -> s
            



    type Equi = 
      { EquipmentNumber : EquipmentCode
        Description : string
        FuncLoc : FuncLocPath
        Attributes : AssocList<string, string>
      }

    let assocsToEqui (attributes : AssocList<string, string>) : Result<Equi, ErrMsg> = 
        match AssocList.tryFind3 "EQUI" "TXTMI" "TPLN_EILO" attributes with
        | Some (number, desc, funcloc) -> 
            Ok { EquipmentNumber = EquipmentCode number
                 Description = desc
                 FuncLoc = FuncLocPath.Create funcloc 
                 Attributes = attributes }
         | None -> Error "Could not find required fields for a Equi"

    /// Note - CharacteristicValue is used three times.
    let equiToAssocs (equi: Equi) : AssocList<string, string> = 
        equi.Attributes
            |> AssocList.upsert "EQUI"          equi.EquipmentNumber.Code
            |> AssocList.upsert "TXTMI"         equi.Description
            |> AssocList.upsert "TPLN_EILO"     (equi.FuncLoc.ToString()) 

    let readEquiChangeFile (inputFile : string) : CompilerMonad<Equi list, 'env> = 
        compile { 
            let! ast = liftResult (readChangeFile inputFile) |>> ofChangeFile
            return! mapM (liftResult << assocsToEqui) ast.Rows
        }

    // ************************************************************************
    // ClassEqui
    
    type ClassEqui = 
        { EquipmentNumber : EquipmentCode
          Class : string
          ClassType : IntegerString
          ClassNumber : IntegerString
          Status : int
        }

    let assocsToClassEqui (attributes : AssocList<string, string>) : Result<ClassEqui, ErrMsg> = 
        match AssocList.tryFind5 "EQUI" "CLASS" "CLASSTYPE" "CLINT" "CLSTATUS1" attributes with
        | Some (number, claz, claztype, clint, status) -> 
            Ok { EquipmentNumber = EquipmentCode number
                 Class = claz
                 ClassType = IntegerString.OfString claztype
                 ClassNumber = IntegerString.OfString clint
                 Status = int status }
         | None -> Error "Could not find required fields for a ClassEqui"

    let classEquiToAssocs (classEqui: ClassEqui) : AssocList<string, string> = 
        AssocList.ofList
            [ ("EQUI",          classEqui.EquipmentNumber.Code)
            ; ("CLASS",         classEqui.Class)
            ; ("CLASSTYPE",     classEqui.ClassType.Number)
            ; ("CLINT",         classEqui.ClassNumber.Number)
            ; ("CLSTATUS1",     classEqui.Status.ToString())
            ]

    let readClassEquiChangeFile (inputFile : string) : CompilerMonad<ClassEqui list, 'env> = 
        compile { 
            let! ast = liftResult (readChangeFile inputFile) |>> ofChangeFile
            return! mapM (liftResult << assocsToClassEqui) ast.Rows
        }

    // ************************************************************************
    // ValuaEqui


    /// ValueCount is the number of instances for this charcteristic 
    /// in a class.
    type ValuaEqui = 
        { EquipmentNumber : EquipmentCode
          ClassType : IntegerString
          CharacteristicID : string
          CharacteristicValue : string
          ValueCount : int
          Attributes : AssocList<string, string>
        }
    
    let assocsToValuaEqui (attributes : AssocList<string, string>) : Result<ValuaEqui, ErrMsg> = 
        match AssocList.tryFind5 "EQUI" "CLASSTYPE" "CHARID" "ATWRT" "VALCNT" attributes with
        | Some (number, claztype, cid, cvalue, count) -> 
            Ok { EquipmentNumber = EquipmentCode number
                 ClassType = IntegerString.OfString claztype
                 CharacteristicID = cid
                 CharacteristicValue = cvalue
                 ValueCount = int count
                 Attributes = attributes }
         | None -> Error "Could not find required fields for a ValuaEqui"

    /// Note - CharacteristicValue is used twice.
    let valuaEquiToAssocs (valua: ValuaEqui) : AssocList<string, string> = 
        valua.Attributes
            |> AssocList.upsert "EQUI"          valua.EquipmentNumber.Code
            |> AssocList.upsert "CLASSTYPE"     valua.ClassType.Number
            |> AssocList.upsert "CHARID"        valua.CharacteristicID
            |> AssocList.upsert "ATWRT"         valua.CharacteristicValue
            |> AssocList.upsert "TEXTBEZ"       valua.CharacteristicValue
            |> AssocList.upsert "VALCNT"        (sprintf "%04i" valua.ValueCount)
            

    let readValuaEquiChangeFile (inputFile : string) : CompilerMonad<ValuaEqui list, 'env> = 
        compile { 
            let! ast = liftResult (readChangeFile inputFile) |>> ofChangeFile
            return! mapM (liftResult << assocsToValuaEqui) ast.Rows
        }