// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module PatchTypes =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
   
    type ObjectStatus = 
        | Operational
        | UnderConstruction

    
    let dateDefault : DateTime = 
        new DateTime(year = 1970, month = 1, day = 1)

    let makeAssocs (items : (string * string * string) list) : AssocList<string,string> = 
        items |> List.map (fun (x,_,y) -> (x,y)) |> AssocList.ofList



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
    
    /// Note - more params need to be added when we understand the data
    /// e.g CompanyCode, ControllingArea ...
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
        let  parent1 = 
            match funcLoc.Path |> parent with
            | None -> ""
            | Some path -> path.ToString()
        makeAssocs
            [ ("ABCKZFLOC",     "ABC Indicator",                    "")
            ; ("GSBE_FLOC",     "Business Area",                    "")
            ; ("BUKRSFLOC",     "Company Code",                     "2100")
            ; ("KOKR_FLOC",     "Controlling Area",                 "1000")
            ; ("KOST_FLOC",     "Cost Center",                      "")    
            ; ("TXTMI",         "Description (medium text)",        funcLoc.Description)
            ; ("FLTYP",         "FuncLocCategory",                  funcLoc.Category.ToString())
            ; ("FUNCLOC",       "Function Location",                "")     // Must be blank
            ; ("IEQUI",         "Installation allowed",             "")
            ; ("STOR_FLOC",     "Location",                         "")
            ; ("STORTI",        "Location origin",                  "D")
            ; ("GEWRKFLOC",     "Main work center",                 "DEFAULT")
            ; ("INGR_FLOC",     "Maint Planner Group",              "")
            ; ("SWERK_FL",      "Maintenance Plant",                "2100")
            ; ("FLOC_REF",      "Masked Functional Location",       funcLoc.Path.ToString())
            ; ("OBJTYFLOC",     "Object Type",                      "A")
            ; ("EQART",         "Object Type",                      funcLoc.ObjectType)
            ; ("PLNT_FLOC",     "Planning Plant",                   "2100")
            ; ("BEBER_FL",      "Plant Section",                    "")
            ; ("WERGWFLOC",     "Plant for WorkCenter",             "2100")
            ; ("TRPNR",         "Reference Location",               "")
            ; ("INBDT",         "Start-up date",                    funcLoc.StartupDate |> showS4Date)
            ; ("STATTEXT",      "Status",                           "CRTE")
            ; ("USTW_FLOC",     "Status of an object",              funcLoc.ObjectStatus)
            ; ("USWO_FLOC",     "Status without status number",     "")
            ; ("TPLKZ_FLC",     "Structure indicator",              funcLoc.StructureIndicator)
            ; ("TPLMA1",        "Superior FL for CR Processing",    parent1)
            ; ("TPLMA",         "Superior FunctLoc",                parent1)
            ; ("PROI_FLOC",     "WBS Element",                      "")
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
        makeAssocs
            [ ("FUNCLOC",       "Functional Location",      classFloc.FuncLoc.ToString())
            ; ("CLASS",         "Class",                    classFloc.Class)
            ; ("CLASSTYPE",     "Class Type",               classFloc.ClassType.Number)
            ; ("CLSTATUS1",     "Status",                   classFloc.Status.ToString())
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
        makeAssocs
            [ ("FUNCLOC",       "Function Location",                valua.FuncLoc.ToString())
            ; ("CLASSTYPE",     "Class Type",                       valua.ClassType.Number)
            ; ("CHARID",        "Characteristic ID",                valua.CharacteristicID)
            ; ("ATWRT",         "Characteristic Value",             valua.CharacteristicValue)
            ; ("TEXTBEZ",       "Description",                      valua.CharacteristicValue)
            ; ("VALCNT",        "Int count values",                 sprintf "%04i" valua.ValueCount)
            ; ("ATFLV",         "Value from",                       valua.CharacteristicValue)
            ]



    // ************************************************************************
    // Equi


    type Equi = 
      { EquipmentNumber : string     // This is not a valid S4 EQUI code
        Description : string
        FuncLoc : FuncLocPath
        Category : string           // e.g. I for instrument
        ObjectType : string
        Manufacturer : string
        Model : string
        SerialNumber : string
        ConstructionYear : uint16
        ConstructionMonth : uint8
        StartupDate : DateTime
        MaintenancePlant : uint32
      }

    /// Note - do not write EQUI to file.
    let equiToAssocs (equi: Equi) : AssocList<string, string> =         
        makeAssocs
            // [ ("ABCK_EILO",     "ABC Indicator",                    "") 
            // ; ("GSBE_EILO",     "Business Area",                    "")
            [ ("BUKR_EILO",     "Company Code",                     "2100")
            ; ("BAUMM_EQI",     "Construction month",               (sprintf "%02i" equi.ConstructionMonth))
            ; ("BAUJJ",         "Construction year",                equi.ConstructionYear.ToString())

            ; ("KOKR_EILO",     "Controlling Area",                 "1000")
            // ; ("KOST_EILO",     "Cost Center",                      "150008")   /// <--- This should be a param
            ; ("TXTMI",         "Description (medium text)",        equi.Description)  
            // ; ("USTA_EQUI",     "Display lines for user status",    "OPER")
            
            ; ("EQTYP",         "Equipment category",               equi.Category)   
            ; ("TPLN_EILO",     "Functional Location",              equi.FuncLoc.ToString())
            // ; ("STOR_EILO",     "Location",                         "")
            // ; ("STORTI",        "Location origin",                  "")
            // ; ("ARBP_EEQZ",     "Main work center",                 "DEFAULT")
            // ; ("INGR_EEQZ",     "Maint. Planner Group",             "")
            // ; ("SWER_EILO",     "Maintenance Plant",                equi.MaintenancePlant.ToString()) 
            ; ("SERGE",         "ManufSerialNumber",                equi.SerialNumber)
            ; ("HERST",         "Manufacturer",                     equi.Manufacturer)
            ; ("TYPBZ",         "Model number",                     equi.Model)
            ; ("OBJT_EQUI",     "Object Type",                      "")
            ; ("EQART_EQU",     "Object Type",                      equi.ObjectType)
            // ; ("PPLA_EEQZ",     "Planning Plant",                   "2100")
            // ; ("BEBE_EILO",     "Plant Section",                    "")
            // ; ("WERGW_EQI",     "Plant for WorkCenter",             "2100")
            ; ("INBDT",         "Start-up date",                    equi.StartupDate |> showS4Date)
            //; ("STATTEXT",      "Status",                           "CRTE")
            ; ("USTW_EQUI",     "Status of an object",              "UCON")
            // ; ("USWO_EQUI",     "Status without status number",     "")
            // ; ("PROI_EILO",     "WBS Element",                      "")
            ]

    // ************************************************************************
    // ClassEqui
    
    type ClassEqui = 
        { EquipmentNumber : string
          Class : string
          ClassType : IntegerString
          ClassNumber : IntegerString
          Status : int
        }

    let classEquiToAssocs (classEqui: ClassEqui) : AssocList<string, string> = 
        // Don't print CLINT (Internal class no)
        makeAssocs
            [ ("EQUI",          "Equipment",                classEqui.EquipmentNumber)
            ; ("CLASS",         "Class",                    classEqui.Class)
            ; ("CLASSTYPE",     "Class Type",               classEqui.ClassType.Number)
            ; ("CLSTATUS1",     "Status",                   classEqui.Status.ToString())
            ]


    // ************************************************************************
    // ValuaEqui


    /// ValueCount is the number of instances for this charcteristic 
    /// in a class.
    type ValuaEqui = 
        { EquipmentNumber : string
          ClassType : IntegerString
          CharacteristicID : string
          CharacteristicValue : string
          ValueCount : int
        }
    

    /// Note - CharacteristicValue is used twice.
    let valuaEquiToAssocs (valua: ValuaEqui) : AssocList<string, string> = 
        makeAssocs
            [ ("EQUI",          "Equipment",                valua.EquipmentNumber)
            ; ("CLASSTYPE",     "Class Type",               valua.ClassType.Number)
            ; ("CHARID",        "Characteristic ID",        valua.CharacteristicID)
            ; ("ATWRT",         "Characteristic Value",     valua.CharacteristicValue)
            ; ("TEXTBEZ",       "Description",              valua.CharacteristicValue)
            ; ("VALCNT",        "Int counter values",       sprintf "%04i" valua.ValueCount)
            ]
            
