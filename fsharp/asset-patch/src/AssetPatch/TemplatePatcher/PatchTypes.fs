// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module PatchTypes =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.AbsChangeFile
    open AssetPatch.Base.Parser
    open AssetPatch.Base.FuncLocPath
   
    type ObjectStatus = 
        | Operational
        | UnderConstruction

    
    let dateDefault : DateTime = 
        new DateTime(year = 1970, month = 1, day = 1)

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
        Category : uint32
        ObjectStatus : string
        StartupDate : DateTime
        StructureIndicator : string
      }
        member x.Level with get () : int = x.Path.Level

        

    let funcLocToAssocs (funcLoc: FuncLoc) : AssocList<string, string> = 
        let parent1 = 
            match funcLoc.Path |> parent with
            | None -> ""
            | Some path -> path.ToString()
        AssocList.empty
            |> AssocList.upsert "FUNCLOC"       (funcLoc.Path.ToString())
            |> AssocList.upsert "TXTMI"         funcLoc.Description
            |> AssocList.upsert "FLTYP"         (funcLoc.Category.ToString())
            |> AssocList.upsert "EQART"         funcLoc.ObjectType
            |> AssocList.upsert "INBDT"         (funcLoc.StartupDate |> showS4Date)
            |> AssocList.upsert "USTW_FLOC"     funcLoc.ObjectStatus
            |> AssocList.upsert "TPLKZ_FLC"     funcLoc.StructureIndicator
            |> AssocList.upsert "TPLMA"         parent1



    let extendFuncLoc (segment : FuncLocSegment)
                      (startupDate : DateTime)
                      (floc: FuncLoc) : FuncLoc = 
        { Path = FuncLocPath.extend segment.Name floc.Path
          Description = segment.Description
          ObjectType = segment.Description
          Category = floc.Category + 1u
          ObjectStatus = "UCON"
          StartupDate = startupDate
          StructureIndicator = floc.StructureIndicator  }


    // ************************************************************************
    // ClassFloc


    type ClassFloc = 
      { FuncLoc : FuncLocPath
        Class : string
        ClassType : IntegerString
        ClassNumber : IntegerString
        Status : int
      }


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
        ValueCount : int
      }


    /// Note - CharacteristicValue is used three times.
    let valuaFlocToAssocs (valua: ValuaFloc) : AssocList<string, string> = 
        AssocList.empty
            |> AssocList.upsert "FUNCLOC"       (valua.FuncLoc.ToString())
            |> AssocList.upsert "CLASSTYPE"     valua.ClassType.Number
            |> AssocList.upsert "CHARID"        valua.CharacteristicID
            |> AssocList.upsert "ATWRT"         valua.CharacteristicValue
            |> AssocList.upsert "TEXTBEZ"       valua.CharacteristicValue
            |> AssocList.upsert "ATFLV"         valua.CharacteristicValue
            |> AssocList.upsert "VALCNT"        (sprintf "%04i" valua.ValueCount)



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
        ObjectType : string
        Manufacturer : string
        Model : string
        StartupDate : DateTime
        MaintenancePlant : uint32
      }

    /// Note - CharacteristicValue is used three times.
    let equiToAssocs (equi: Equi) : AssocList<string, string> = 
        AssocList.empty
            |> AssocList.upsert "EQUI"          equi.EquipmentNumber.Code
            |> AssocList.upsert "TXTMI"         equi.Description
            |> AssocList.upsert "TPLN_EILO"     (equi.FuncLoc.ToString()) 
            |> AssocList.upsert "EQART_EQU"     equi.ObjectType
            |> AssocList.upsert "SWER_EILO"     (equi.MaintenancePlant.ToString())
            |> AssocList.upsert "INBDT"         (equi.StartupDate |> showS4Date)


    // ************************************************************************
    // ClassEqui
    
    type ClassEqui = 
        { EquipmentNumber : EquipmentCode
          Class : string
          ClassType : IntegerString
          ClassNumber : IntegerString
          Status : int
        }

    let classEquiToAssocs (classEqui: ClassEqui) : AssocList<string, string> = 
        AssocList.ofList
            [ ("EQUI",          classEqui.EquipmentNumber.Code)
            ; ("CLASS",         classEqui.Class)
            ; ("CLASSTYPE",     classEqui.ClassType.Number)
            ; ("CLINT",         classEqui.ClassNumber.Number)
            ; ("CLSTATUS1",     classEqui.Status.ToString())
            ]


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
    

    /// Note - CharacteristicValue is used twice.
    let valuaEquiToAssocs (valua: ValuaEqui) : AssocList<string, string> = 
        valua.Attributes
            |> AssocList.upsert "EQUI"          valua.EquipmentNumber.Code
            |> AssocList.upsert "CLASSTYPE"     valua.ClassType.Number
            |> AssocList.upsert "CHARID"        valua.CharacteristicID
            |> AssocList.upsert "ATWRT"         valua.CharacteristicValue
            |> AssocList.upsert "TEXTBEZ"       valua.CharacteristicValue
            |> AssocList.upsert "VALCNT"        (sprintf "%04i" valua.ValueCount)
            
