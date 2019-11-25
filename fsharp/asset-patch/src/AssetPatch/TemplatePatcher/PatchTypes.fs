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
        //let parent1 = 
        //    match funcLoc.Path |> parent with
        //    | None -> ""
        //    | Some path -> path.ToString()
        AssocList.ofList
            [ ("ABCKZFLOC",     "")
            ; ("GSBE_FLOC",     "")
            ; ("BUKRSFLOC",     "2100")
            ; ("KOKR_FLOC",     "1000")
            ; ("KOST_FLOC",     "")    
            ; ("TXTMI",         funcLoc.Description)
            ; ("FLTYP",         funcLoc.Category.ToString())
            ; ("FUNCLOC",       funcLoc.Path.ToString())
            ; ("IEQUI",         "")
            ; ("STOR_FLOC",     "")
            ; ("STORTI",        "D")
            ; ("GEWRKFLOC",    "DEFAULT")
            ; ("INGR_FLOC",     "")
            ; ("SWERK_FL",      "2100")
            ; ("FLOC_REF",      funcLoc.Path.ToString())
            ; ("OBJTYFLOC",     "A")
            ; ("EQART",         funcLoc.ObjectType)
            ; ("PLNT_FLOC",     "2100")
            ; ("BEBER_FL",      "")
            ; ("WERGWFLOC",     "2100")
            ; ("TRPNR",         "")
            ; ("INBDT",         funcLoc.StartupDate |> showS4Date)
            ; ("STATTEXT",      "CRTE")
            ; ("USTW_FLOC",     funcLoc.ObjectStatus)
            ; ("USWO_FLOC",     "")
            ; ("TPLKZ_FLC",     funcLoc.StructureIndicator)
            ; ("PROI_FLOC",     "")
            ]
            


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
        // Dont print the CLINT even though we know it!
        AssocList.ofList
            [ ("FUNCLOC",       classFloc.FuncLoc.ToString())
            ; ("CLASS",         classFloc.Class)
            ; ("CLASSTYPE",     classFloc.ClassType.Number)
            ; ("CLINT",         "")
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
        AssocList.ofList
            [ ("FUNCLOC",       valua.FuncLoc.ToString())
            ; ("CLASSTYPE",     valua.ClassType.Number)
            ; ("CHARID",        valua.CharacteristicID)
            ; ("ATWRT",         valua.CharacteristicValue)
            ; ("TEXTBEZ",       valua.CharacteristicValue)
            ; ("VALCNT",        sprintf "%04i" valua.ValueCount)
            ; ("ATFLV",         valua.CharacteristicValue)
            ]



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
        Category : string           // e.g. I for instrument
        ObjectType : string
        Manufacturer : string
        Model : string
        SerialNumber : string
        StartupDate : DateTime
        MaintenancePlant : uint32
      }

    /// Note - CharacteristicValue is used three times.
    let equiToAssocs (equi: Equi) : AssocList<string, string> = 
        AssocList.ofList
            [ ("ABCK_EILO",     "")
            ; ("GSBE_EILO",     "")
            ; ("BUKR_EILO",     "")
            ; ("KOKR_EILO",     "1000")
            ; ("KOST_EILO",     "150008")               /// <--- This should be _data_
            ; ("TXTMI",         equi.Description)       // Description (medium) text
            ; ("USTA_EQUI",     "OPER")                 // Display lines for user status
            ; ("EQUI",          equi.EquipmentNumber.Code)      // Equipment
            ; ("EQTYP",         equi.Category)   
            ; ("TPLN_EILO",     equi.FuncLoc.ToString())
            ; ("STOR_EILO",     "")
            ; ("STORTI",        "")
            ; ("ARBP_EEQZ",     "DEFAULT")
            ; ("INGR_EEQZ",     "")
            ; ("SWER_EILO",     equi.MaintenancePlant.ToString())   // Maintenance Plant
            ; ("SERGE",         equi.SerialNumber)                  // ManufSerialNumber
            ; ("HERST",         equi.Manufacturer)                  // Manufacturer
            ; ("TYPBZ",         equi.Model)
            ; ("OBJT_EQUI",     "")                     // Object Type
            ; ("EQART_EQU",     equi.ObjectType)        // Object Type
            ; ("PPLA_EEQZ",     "2100")                 // Planning Plant
            ; ("BEBE_EILO",     "")                     // Planet Section
            ; ("WERGW_EQI",     "2100")                 // Plant for WorkCenter
            ; ("INBDT",         equi.StartupDate |> showS4Date)     // Start-up Date
            ; ("STATTEXT",      "CRTE")                 // Status
            ; ("USTW_EQUI",     "UCON")                 // Status of an object
            ; ("USWO_EQUI",     "")                     // Status without status number
            ; ("PROI_EILO",     "")                     // 	WBS Element 
            ]

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
        }
    

    /// Note - CharacteristicValue is used twice.
    let valuaEquiToAssocs (valua: ValuaEqui) : AssocList<string, string> = 
        AssocList.ofList
            [ ("EQUI",          valua.EquipmentNumber.Code)
            ; ("CLASSTYPE",     valua.ClassType.Number)
            ; ("CHARID",        valua.CharacteristicID)
            ; ("ATWRT",         valua.CharacteristicValue)
            ; ("TEXTBEZ",       valua.CharacteristicValue)
            ; ("VALCNT",        sprintf "%04i" valua.ValueCount)
            ]
            
