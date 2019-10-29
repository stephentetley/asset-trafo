// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base



module EntityTypes =
    
    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
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

    let tryAssocsToClassFloc (attributes : AssocList<string, string>) : ClassFloc option = 
        match AssocList.tryFind5 "FUNCLOC" "CLASS" "CLASSTYPE" "CLINT" "CLSTATUS1" attributes with
        | Some (funcloc, claz, claztype, clint, status) -> 
            Some { FuncLoc = FuncLocPath.Create funcloc 
                   Class = claz
                   ClassType = IntegerString.OfString claztype
                   ClassNumber = IntegerString.OfString clint
                   Status = int status
                  }
         | None -> None

    let classFlocToAssocs (classFloc: ClassFloc) : AssocList<string, string> = 
        AssocList.ofList
            [ ("FUNCLOC",       classFloc.FuncLoc.ToString())
            ; ("CLASS",         classFloc.Class)
            ; ("CLASSTYPE",     classFloc.ClassType.Number)
            ; ("CLINT",         classFloc.ClassNumber.Number)
            ; ("CLSTATUS1",     classFloc.Status.ToString())
            ]


    // ************************************************************************
    // ValuaFloc

    type ValuaFloc = 
      { FuncLoc : FuncLocPath
        ClassType : IntegerString
        CharacteristicID : string
        CharacteristicValue : string
        Attributes : AssocList<string, string>
      }

    let tryAssocsToValuaFloc (attributes : AssocList<string, string>) : ValuaFloc option = 
        match AssocList.tryFind4 "FUNCLOC" "CLASSTYPE" "CHARID" "ATWRT" attributes with
        | Some (funcloc, claztype, cid, cvalue) -> 
            Some { FuncLoc = FuncLocPath.Create funcloc 
                   ClassType = IntegerString.OfString claztype
                   CharacteristicID = cid
                   CharacteristicValue = cvalue
                   Attributes = attributes
                  }
         | None -> None

    /// Note - CharacteristicValue is used three times.
    let valuaFlocToAssocs (valuaFloc: ValuaFloc) : AssocList<string, string> = 
        valuaFloc.Attributes
            |> AssocList.update "FUNCLOC"       (valuaFloc.FuncLoc.ToString())
            |> AssocList.update "CLASSTYPE"     valuaFloc.ClassType.Number
            |> AssocList.update "CHARID"        valuaFloc.CharacteristicID
            |> AssocList.update "ATWRT"         valuaFloc.CharacteristicValue
            |> AssocList.update "TEXTBEZ"       valuaFloc.CharacteristicValue
            |> AssocList.update "ATFLV"         valuaFloc.CharacteristicValue

    // ************************************************************************
    // Equi

    type Equi = 
      { EquipmentNumber : IntegerString
        Description : string
        FuncLoc : FuncLocPath
        Attributes : AssocList<string, string>
      }

    let tryAssocsToEqui (attributes : AssocList<string, string>) : Equi option = 
        match AssocList.tryFind3 "EQUI" "TXTMI" "TPLN_EILO" attributes with
        | Some (number, funcloc, desc) -> 
            Some { EquipmentNumber = IntegerString.OfString number
                   Description = desc
                   FuncLoc = FuncLocPath.Create funcloc 
                   Attributes = attributes
                  }
         | None -> None

    /// Note - CharacteristicValue is used three times.
    let equiToAssocs (equi: Equi) : AssocList<string, string> = 
        equi.Attributes
            |> AssocList.update "EQUI"          equi.EquipmentNumber.Number
            |> AssocList.update "TXTMI"         equi.Description
            |> AssocList.update "TPLN_EILO"     (equi.FuncLoc.ToString()) 

    // ************************************************************************
    // ClassEqui
    
    type ClassEqui = 
        { EquipmentNumber : IntegerString
          Class : string
          ClassType : IntegerString
          ClassNumber : IntegerString
          Status : int
        }

    let tryAssocsToClassEqui (attributes : AssocList<string, string>) : ClassEqui option = 
        match AssocList.tryFind5 "EQUI" "CLASS" "CLASSTYPE" "CLINT" "CLSTATUS1" attributes with
        | Some (number, claz, claztype, clint, status) -> 
            Some { EquipmentNumber = IntegerString.OfString number
                   Class = claz
                   ClassType = IntegerString.OfString claztype
                   ClassNumber = IntegerString.OfString clint
                   Status = int status
                  }
         | None -> None

    let classEquiToAssocs (classEqui: ClassEqui) : AssocList<string, string> = 
        AssocList.ofList
            [ ("EQUI",          classEqui.EquipmentNumber.Number)
            ; ("CLASS",         classEqui.Class)
            ; ("CLASSTYPE",     classEqui.ClassType.Number)
            ; ("CLINT",         classEqui.ClassNumber.Number)
            ; ("CLSTATUS1",     classEqui.Status.ToString())
            ]


    // ************************************************************************
    // ValuaEqui

    type ValuaEqui = 
        { EquipmentNumber : IntegerString
          ClassType : IntegerString
          CharacteristicID : string
          CharacteristicValue : string
          Attributes : AssocList<string, string>
        }
    
    let tryAssocsToValuaEqui (attributes : AssocList<string, string>) : ValuaEqui option = 
        match AssocList.tryFind4 "EQUI" "CLASSTYPE" "CHARID" "ATWRT" attributes with
        | Some (number, claztype, cid, cvalue) -> 
            Some { EquipmentNumber = IntegerString.OfString number
                   ClassType = IntegerString.OfString claztype
                   CharacteristicID = cid
                   CharacteristicValue = cvalue
                   Attributes = attributes
                  }
         | None -> None

    /// Note - CharacteristicValue is used three times.
    let valuaEquiToAssocs (valuaEqui: ValuaEqui) : AssocList<string, string> = 
        valuaEqui.Attributes
            |> AssocList.update "EQUI"          valuaEqui.EquipmentNumber.Number
            |> AssocList.update "CLASSTYPE"     valuaEqui.ClassType.Number
            |> AssocList.update "CHARID"        valuaEqui.CharacteristicID
            |> AssocList.update "ATWRT"         valuaEqui.CharacteristicValue
            |> AssocList.update "TEXTBEZ"       valuaEqui.CharacteristicValue
            |> AssocList.update "ATFLV"         valuaEqui.CharacteristicValue