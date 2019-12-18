// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module PatchTypes =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
   
    open AssetPatch.TemplatePatcher.CommonTypes


    let private makeAssocs (items : (string * string * string) list) : AssocList<string,string> = 
        items |> List.map (fun (x,_,y) -> (x,y)) |> AssocList.ofList



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
    
    /// Shared by Floc and Equi
    /// Note - more params may need to be added when we better understand the data
    type CommonProperties = 
        { CompanyCode : uint32
          ControllingArea : uint32
          PlantCode : uint32
          UserStatus : string
        }

    // ************************************************************************
    // Create FuncLoc

    type NewFuncLoc = 
      { FunctionLocation : FuncLocPath
        Description : string
        ObjectType : string
        Category : uint32
        ObjectStatus : string
        StartupDate : DateTime
        StructureIndicator : string
        CommonProps : CommonProperties
      }
        member x.Level with get () : int = x.FunctionLocation.Level

        member x.ToAssocs() : AssocList<string, string> =  
            let installationAllowed = 
                if x.FunctionLocation.Level >= 5 then "X" else ""
            makeAssocs
                [ ("ABCKZFLOC",     "ABC Indicator",                    "")
                ; ("GSBE_FLOC",     "Business Area",                    "")
                ; ("BUKRSFLOC",     "Company Code",                     x.CommonProps.CompanyCode.ToString())
                ; ("KOKR_FLOC",     "Controlling Area",                 x.CommonProps.ControllingArea.ToString())
                ; ("KOST_FLOC",     "Cost Center",                      "")    
                ; ("TXTMI",         "Description (medium text)",        x.Description)
                ; ("USTA_FLOC",     "Display lines for user status",    x.CommonProps.UserStatus)
                ; ("FLTYP",         "FuncLocCategory",                  x.Category.ToString())
                ; ("FUNCLOC",       "Function Location",                "")     // Must be blank
                ; ("IEQUI",         "Installation allowed",             installationAllowed)
                ; ("STOR_FLOC",     "Location",                         "")
                ; ("GEWRKFLOC",     "Main work center",                 "DEFAULT")
                ; ("INGR_FLOC",     "Maint Planner Group",              "")
                ; ("SWERK_FL",      "Maintenance Plant",                x.CommonProps.PlantCode.ToString())
                ; ("FLOC_REF",      "Masked Functional Location",       x.FunctionLocation.ToString())
                ; ("OBJTYFLOC",     "Object Type",                      "")
                ; ("EQART",         "Object Type",                      x.ObjectType)
                ; ("PLNT_FLOC",     "Planning Plant",                   x.CommonProps.PlantCode.ToString())
                ; ("BEBER_FL",      "Plant Section",                    "")
                ; ("WERGWFLOC",     "Plant for WorkCenter",             x.CommonProps.PlantCode.ToString())
                ; ("TRPNR",         "Reference Location",               "")
                ; ("INBDT",         "Start-up date",                    x.StartupDate |> showS4Date)
                ; ("STATTEXT",      "Status",                           "CRTE")
                ; ("STSM_FLOC",     "Status Profile",                   "ZFLOCST")
                ; ("USTW_FLOC",     "Status of an object",              x.ObjectStatus)
                ; ("USWO_FLOC",     "Status without status number",     "")
                ; ("TPLKZ_FLC",     "Structure indicator",              x.StructureIndicator)
                ; ("TPLMA1",        "Superior FL for CR Processing",    "")
                ; ("TPLMA",         "Superior FunctLoc",                "")
                ; ("PROI_FLOC",     "WBS Element",                      "")
                ; ("ARBPLFLOC",     "Work center",                      "DEFAULT")
                ]
            
    // ************************************************************************
    // Link FuncLoc

    type LinkFuncLoc = 
        { FunctionLocation : FuncLocPath
          Description : string
          ObjectType : string
          Category : uint32
          ObjectStatus : string
          StartupDate : DateTime
          StructureIndicator : string
        }

        member x.ToAssocs() : AssocList<string, string> = 
            let parent1 = 
                match x.FunctionLocation |> parent with
                | None -> ""
                | Some path -> path.ToString()
            makeAssocs
                [ ("FUNCLOC",       "Function Location",                x.FunctionLocation.ToString())
                ; ("TXTMI",         "Description (medium text)",        x.Description)
                ; ("FLTYP",         "FuncLocCategory",                  x.Category.ToString())
                ; ("OBJTYFLOC",     "Object Type",                      "")
                ; ("EQART",         "Object Type",                      x.ObjectType)
                ; ("INBDT",         "Start-up date",                    x.StartupDate |> showS4Date)
                ; ("USTW_FLOC",     "Status of an object",              x.ObjectStatus)
                ; ("TPLKZ_FLC",     "Structure indicator",              x.StructureIndicator)
                ; ("TPLMA1",        "Superior FL for CR Processing",    parent1)
                ; ("TPLMA",         "Superior FunctLoc",                parent1)
                ]
    

    // ************************************************************************
    // ClassFloc


    type NewClassFloc = 
        { FuncLoc : FuncLocPath
          Class : string
          Status : int
        }

        member x.ToAssocs() : AssocList<string, string> =      
            makeAssocs
                [ ("FUNCLOC",       "Functional Location",      x.FuncLoc.ToString())
                ; ("CLASS",         "Class",                    x.Class)
                ; ("CLASSTYPE",     "Class Type",               "003")
                ; ("CLSTATUS1",     "Status",                   x.Status.ToString())
                ]


    // ************************************************************************
    // ValuaFloc

    type NewValuaFloc = 
        { FuncLoc : FuncLocPath
          ClassType : IntegerString
          CharacteristicID : string        
          ValueCount : int
          Value : ValuaValue
        }


        /// Note - Numeric values print TEXTBEZ and ATFLV
        /// textual values print ATWRT and TEXTBEZ
        member x.ToAssocs() : AssocList<string, string> =   
            makeAssocs
                [ ("FUNCLOC",       "Function Location",                x.FuncLoc.ToString())
                ; ("CLASSTYPE",     "Class Type",                       x.ClassType.Number)
                ; ("CHARID",        "Characteristic ID",                x.CharacteristicID)
                ; ("ATWRT",         "Characteristic Value",             characteristicValueOutput x.Value)
                ; ("ATCOD",         "Code",                             "1")        // Always 1 "EQ"
                ; ("TEXTBEZ",       "Description",                      x.Value.ToString())
                ; ("VALCNT",        "Int count values",                 sprintf "%04i" x.ValueCount)
                ; ("ATFLV",         "Value from",                       valueFromOutput x.Value)
                ; ("ATFLB",         "Value to",                         valueToOutput x.Value)
                ]



    // ************************************************************************
    // Equi


    type NewEqui = 
        { Description : string
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
          Currency : string
          CommonProps : CommonProperties
        }

        /// Note - do not write EQUI to file.
        member x.ToAssocs() : AssocList<string, string> =         
            makeAssocs
                // [ ("ABCK_EILO",     "ABC Indicator",                    "") 
                // ; ("GSBE_EILO",     "Business Area",                    "")
                [ ("BUKR_EILO",     "Company Code",                     x.CommonProps.CompanyCode.ToString())
                ; ("BAUMM_EQI",     "Construction month",               (sprintf "%02i" x.ConstructionMonth))
                ; ("BAUJJ",         "Construction year",                x.ConstructionYear.ToString())

                ; ("KOKR_EILO",     "Controlling Area",                 x.CommonProps.ControllingArea.ToString())
                // ; ("KOST_EILO",     "Cost Center",                      "150008")   /// <--- This should be a param
                ; ("WAERS",         "Currency",                         x.Currency)
                ; ("TXTMI",         "Description (medium text)",        x.Description)  
                ; ("USTA_EQUI",     "Display lines for user status",    x.CommonProps.UserStatus)
            
                ; ("EQTYP",         "Equipment category",               x.Category)   
                ; ("TPLN_EILO",     "Functional Location",              x.FuncLoc.ToString())
                // ; ("STOR_EILO",     "Location",                         "")
                // ; ("STORTI",        "Location origin",                  "")
                // ; ("ARBP_EEQZ",     "Main work center",                 "DEFAULT")
                // ; ("INGR_EEQZ",     "Maint. Planner Group",             "")
                // ; ("SWER_EILO",     "Maintenance Plant",                equi.MaintenancePlant.ToString()) 
                ; ("SERGE",         "ManufSerialNumber",                x.SerialNumber)
                ; ("HERST",         "Manufacturer",                     x.Manufacturer)
                ; ("TYPBZ",         "Model number",                     x.Model)
                ; ("OBJT_EQUI",     "Object Type",                      "")
                ; ("EQART_EQU",     "Object Type",                      x.ObjectType)
                // ; ("PPLA_EEQZ",     "Planning Plant",                   "2100")
                // ; ("BEBE_EILO",     "Plant Section",                    "")
                // ; ("WERGW_EQI",     "Plant for WorkCenter",             "2100")
                ; ("INBDT",         "Start-up date",                    x.StartupDate |> showS4Date)
                //; ("STATTEXT",      "Status",                           "CRTE")
                ; ("USTW_EQUI",     "Status of an object",              "UCON")
                // ; ("USWO_EQUI",     "Status without status number",     "")
                // ; ("PROI_EILO",     "WBS Element",                      "")
                ]

    // ************************************************************************
    // ClassEqui
    


    type NewClassEqui = 
        { EquipmentId : string
          Class : string
          Status : int
        }

        member x.ToAssocs() : AssocList<string, string> = 
            makeAssocs
                [ ("EQUI",          "Equipment",                x.EquipmentId.ToString())
                ; ("CLASS",         "Class",                    x.Class)
                ; ("CLASSTYPE",     "Class Type",               "002")
                ; ("CLSTATUS1",     "Status",                   x.Status.ToString())
                ]


    // ************************************************************************
    // ValuaEqui


    /// ValueCount is the number of instances for this charcteristic 
    /// in a class.
    type NewValuaEqui = 
        { EquipmentId : string
          ClassType : IntegerString
          CharacteristicID : string
          ValueCount : int
          Value : ValuaValue
        }
    

        /// Note - CharacteristicValue is used twice.
        member x.ToAssocs() : AssocList<string, string> = 
            makeAssocs
                [ ("EQUI",          "Equipment",                x.EquipmentId.ToString())
                ; ("CLASSTYPE",     "Class Type",               x.ClassType.Number)
                ; ("CHARID",        "Characteristic ID",        x.CharacteristicID)
                ; ("ATWRT",         "Characteristic Value",     characteristicValueOutput x.Value)
                ; ("ATCOD",         "Code",                     "1")        // Always 1 "EQ"
                ; ("TEXTBEZ",       "Description",              x.Value.ToString())
                ; ("VALCNT",        "Int counter values",       sprintf "%04i" x.ValueCount)
                ; ("ATFLV",         "Value from",               valueFromOutput x.Value)
                ; ("ATFLB",         "Value to",                 valueToOutput x.Value)
                ]
            
