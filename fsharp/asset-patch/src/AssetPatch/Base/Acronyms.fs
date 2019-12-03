// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Acronyms = 

    open AssetPatch.Base.ChangeFile

    // ************************************************************************
    // funcloc fields

    let private funclocColumnAcronyms : Map<string,string> = 
        Map.ofList <|
            [ ("ABCKZFLOC",     "ABC Indicator")
            ; ("ABCKZI",        "ABC Indic. Origin")
            ; ("ANSWT",         "Acquisition Value")
            ; ("ANSDT",         "Acquisition date")
            ; ("DEACT",         "Active Indicator Status Indicator")
            ; ("ZADRNR",        "Address")
            ; ("ZADRNRI",       "Address origin")
            ; ("ANLN1_FL",      "Asset")
            ; ("ANLNRI",        "Asset no. origin")
            ; ("EINZLI",        "AuthGroup origin")
            ; ("BEGRUI",        "AuthGroup origin")
            ; ("BEGRU",         "AuthorizGroup")
            ; ("CGWLDT_FL",     "Begin Guarantee(C)")
            ; ("VGWLDT_FL",     "Begin Guarantee(V)")
            ; ("GSBE_FLOC",     "Business Area")
            ; ("GSBERI",        "Business area orig.")
            ; ("RBNR_FLOC",     "Catalog Profile")
            ; ("BUKRSI",        "CompCode origin")
            ; ("BUKRSFLOC",     "Company Code")
            ; ("SUBMTI",        "ConstructType origin")
            ; ("BAUMM",         "Contrstuction month")
            ; ("SUBMTIFLO",     "Construction type Material")
            ; ("BAUJJ",         "Construction year")
            ; ("KOKRSI",        "ControlArea origin")
            ; ("KOKR_FLOC",     "Controlling Area")
            ; ("KOST_FLOC",     "Cost Center")
            ; ("KOSTLI",        "Cost center origin")
            ; ("HERLD",         "Country of manufact.")
            ; ("WAERS",         "Currency")
            ; ("LVORM",         "Deletion Indicator")
            ; ("TXTMI",         "Description (medium text)")
            ; ("USTA_FLOC",     "Display lines for user status")
            ; ("VTWEG",         "Distribution Channel")
            ; ("SPART",         "Division")
            ; ("IEQUII",        "EquipInst. Origin")
            ; ("EQUI_FLOC",     "Equipment")
            ; ("FLTYP",         "FuncLocCategory")
            ; ("FUNCLOC",       "Function Location")
            ; ("BRGEW",         "Gross weight")
            ; ("CGAERB_FL",     "Indicator: Pass on warranty(C)")
            ; ("VGAERB_FL",     "Indicator: Pass on warranty(V)")
            ; ("IEQUI",         "Installation allowed")
            ; ("INVNR",         "Inventory number")
            ; ("LIZNR",         "License number")
            ; ("STOR_FLOC",     "Location")
            ; ("STORTI",        "Location origin")
            ; ("GEWRKFLOC",     "Main work center")
            ; ("INGR_FLOC",     "Maint Planner Group")
            ; ("RBNR_I",        "MaintCatalog origin")
            ; ("INGRPI",        "MaintPlGroup origin")
            ; ("SWERKI",        "MaintPlant origin")
            ; ("SWERK_FL",      "Maintenance Plant")
            ; ("SERGE",         "ManufSerialNumber")
            ; ("MAPAR",         "ManufactPartNo.")
            ; ("HERST",         "Manufacturer")
            ; ("FLOC_REF",      "Masked Functional Location")
            ; ("CMGANR_FL",     "Master Warranty(C)")
            ; ("VMGANR_FL",     "Master Warranty(V)")
            ; ("TYPBZ",         "Model number")
            ; ("OBJIDFLOC",     "Object ID")
            ; ("OBJTYFLOC",     "Object Type")
            ; ("EQART",         "Object Type")
            ; ("JOBJN_FL",      "Object Number")
            ; ("PPSIDI",        "PP WrkCenter origin")
            ; ("PLNT_FLOC",     "Planning Plant")
            ; ("BEBER_FL",      "Plant Section")
            ; ("BEBERI",        "Plant Section Origin")
            ; ("WERGWFLOC",     "Plant for WorkCenter")
            ; ("IWERKI",        "Plant plant origin")
            ; ("POSNR",         "Position in object")
            ; ("TRPNR1",        "Reference FL fo CR Processing")
            ; ("TRPNR",         "Reference Location")
            ; ("MSGRP",         "Room")
            ; ("MSGRPI",        "Room number origin")
            ; ("VKORG",         "Sales Organization")
            ; ("VKGRP",         "Sales group")
            ; ("VKBUR",         "Sales office")
            ; ("VKORGI",        "SalesOrg origin")
            ; ("AUFN_FLOC",     "Settlement Order")
            ; ("AUFNRI",        "SettlementOrderOrigin")
            ; ("IFLOT_SRT",     "Shift Report Type")
            ; ("EINZL",         "Single installation")
            ; ("GROES",         "Size/dimension")
            ; ("EQFNR",         "Sort Field")
            ; ("DAUF_FLOC",     "Standing Order")
            ; ("DAUFNI",        "StandingOrderOrigin")
            ; ("INBDT",         "Start-up date")
            ; ("STATTEXT",      "Status")
            ; ("STSM_FLOC",     "Status Profile")
            ; ("USTW_FLOC",     "Status of an object")
            ; ("USWO_FLOC",     "Status without status number")
            ; ("TPLKZ_FLC",     "Structure indicator")
            ; ("ANLA_FL",       "Sub-number")
            ; ("TPLMA1",        "Superior FL for CR Processing")
            ; ("TPLMA",         "Superior FunctLoc")
            ; ("GEWEI",         "Unit of weight")
            ; ("STTXU",         "User Status")
            ; ("DATBI_FLO",     "Valid To")
            ; ("PROI_FLOC",     "WBS Element")
            ; ("PROIDI",        "WBS element origin")
            ; ("CGWLEN_FL",     "Warranty End Date(C)")
            ; ("VGWLEN_FL",     "Warranty End Date(V)")
            ; ("CWAGET_FL",     "Warranty inheritance possible(C)")
            ; ("VWAGET_FL",     "Warranty inheritance possible(V)")
            ; ("ARBPLFLOC",     "Work center")
            ; ("LGWIDI",        "Work center origin")
            ]

    // ************************************************************************
    // classfloc fields

    let private classflocColumnAcronyms : Map<string,string> = 
        Map.ofList <|
            [ ("CLASS",         "Class")
            ; ("CLASSTYPE",     "Class Type")
            ; ("CLINT",         "Internal class no.")
            ; ("CLSTATUS1",     "Status")
            ; ("FUNCLOC",       "Functional Location")
            ]

    // ************************************************************************
    // valualoc fields

    let private valuaflocColumnAcronyms : Map<string,string> = 
        Map.ofList <|
            [ ("ATAW1",         "Alternative unit")
            ; ("ATAWE",         "Alternative unit")
            ; ("ATAUT",         "Author")
            ; ("CHARID",        "Characteristic ID")
            ; ("ATNAM",         "Characteristic Name")
            ; ("ATWRT",         "Characteristic Value")
            ; ("CLASSTYPE",     "Class Type")
            ; ("ATCOD",         "Code")
            ; ("ATVGLART",      "Comp. type")
            ; ("TEXTBEZ",       "Description")
            ; ("FUNCLOC",       "Function Location")
            ; ("ATZIS",         "Instance counter")
            ; ("VALCNT",        "Int count values")
            ; ("ATIMB",         "Internal char no.")
            ; ("ATSRT",         "Position")
            ; ("ATFLV",         "Value from")
            ; ("ATFLB",         "Value to")
            ]    



    // ************************************************************************
    // equi fields

    let private equiColumnAcronyms : Map<string,string> = 
        Map.ofList <|
            [ ("ABCK_EILO",     "ABC Indicator")
            ; ("ABCKZI",        "ABC indic. Origin")
            ; ("ANSWT",         "Acquisition Value")
            ; ("ANSDT",         "Acquisition date")
            ; ("DEACT",         "Active Inactive Status Indicator")
            ; ("ZADRNR",        "Address")
            ; ("ZADRNRI",       "Address origin")
            ; ("ANL1_EILO",     "Asset")
            ; ("BEGRUI",        "AuthGroup origin")
            ; ("BEGRU",         "AuthorizGroup")
            ; ("CHAR2EQUI",     "Batch")
            ; ("CGWLDT_EQ",     "Begin guatantee(C)")
            ; ("VGWLDT_EQ",     "Begin guatantee(V)")
            ; ("GSBE_EILO",     "Business Area")
            ; ("GSBERI",        "Business area orig.")
            ; ("RBNR_EEQZ",     "Catalog Profile")
            ; ("ZZCLASS",       "Class")
            ; ("BUKRSI",        "CompCode origin")
            ; ("BUKR_EILO",     "Company Code")
            ; ("KMATN",         "Configurable material")
            ; ("BAUMM_EQI",     "Construction month")
            ; ("SUBM_EEQZ",     "Construction type Material")
            ; ("BAUJJ",         "Construction year")
            ; ("KOKRSI",        "ControlArea origin")
            ; ("KOKR_EILO",     "Controlling Area")
            ; ("KOST_EILO",     "Cost Center")
            ; ("KOSTLI",        "Cost center origin")
            ; ("HERLD",         "Country of manufact.")
            ; ("WAERS",         "Currency")
            ; ("KUNDE_EQ",      "Current customer")
            ; ("KUNDE",         "Current customer")
            ; ("KUN1_EEQZ",     "Customer")
            ; ("GEWRKI",        "Data origin")
            ; ("LVORM_EQI",     "Deletion Indicator")
            ; ("AULDT_EQI",     "Delivery date")
            ; ("TXTMI",         "Description (medium text)")
            ; ("USTA_EQUI",     "Display lines for user status")
            ; ("VTWEG",         "Distribution Channel")
            ; ("SPART",         "Division")
            ; ("KUN2_EEQZ",     "End customer")
            ; ("EQUI",          "Equipment")
            ; ("EQTYP",         "Equipment category")
            ; ("TPLNR_I",       "FunctLoc. Origin")
            ; ("TPLN_EILO",     "Functional Location")
            ; ("BRGEW",         "Gross Weight")
            ; ("CGAERB_EQ",     "Indicator: Pass on warranty(C)")
            ; ("VGAERB_EQ",     "Indicator: Pass on warranty(V)")
            ; ("INVNR",         "Inventory number")
            ; ("EQASP",         "Language Key")
            ; ("LSERNR",        "Last num. SerialNo")
            ; ("LIZNR",         "License number")
            ; ("STOR_EILO",     "Location")
            ; ("STORTI",        "Location origin")
            ; ("EQ_LTEXT",      "Long Text")
            ; ("ARBP_EEQZ",     "Main work center")
            ; ("INGR_EEQZ",     "Maint. Planner Group")
            ; ("RBNR_I",        "MaintCatalog origin")
            ; ("INGRPI",        "MaintPlGroup origin")
            ; ("SWERKI",        "MaintPlant origin")
            ; ("SWER_EILO",     "Maintenance Plant")
            ; ("SERGE",         "ManufSerialNumber")
            ; ("MAPA_EEQZ",     "ManufactPartNo.")
            ; ("HERST",         "Manufacturer")
            ; ("CMGANR_EQ",     "Master Warranty(C)")
            ; ("VMGANR_EQ",     "Master Warranty(D)")
            ; ("MAT2EQUI",      "Material")
            ; ("MAT2EQUIC",     "Material")
            ; ("MAT_EQU",       "Material")
            ; ("SERNR",         "Material Serial Number")
            ; ("TYPBZ",         "Model number")
            ; ("OBJI_EILO",     "Object ID")
            ; ("OBJT_EQUI",     "Object Type")
            ; ("EQART_EQU",     "Object Type")
            ; ("KUN3_EEQZ",     "Operator")
            ; ("PPSIDI",        "PP WrkCenter origin")
            ; ("PPLA_EEQZ",     "Planning Plant")
            ; ("WERK_EQUI",     "Plant")
            ; ("BEBE_EILO",     "Plant Section")
            ; ("BEBERI",        "Plant Section Origin")
            ; ("WERGW_EQI",     "Plant for WorkCenter")
            ; ("IWERKI",        "Plant plant origin")
            ; ("HEQN_EEQZ",     "Position")
            ; ("KRFKZ",         "Referenced config.")
            ; ("MSGR_EILO",     "Room")
            ; ("MSGRPI",        "Room number origin")
            ; ("VKORG",         "Sales Organization")
            ; ("VKGRP",         "Sales group")
            ; ("VKBUR",         "Sales office")
            ; ("VKORGI",        "SalesOrg. Origin")
            ; ("GERNR",         "Serial Number")
            ; ("AUFN_EILO",     "Settlement Order")
            ; ("AUFNRI",        "SettlemntOrderOrigin")
            ; ("GROES_EQU",     "Size/dimension")
            ; ("EQFN_EILO",     "Sort Field")
            ; ("EQFNRI",        "Sort field origin")
            ; ("DAUF_EILO",     "Standing Order")
            ; ("DAUFNI",        "StandingOrderOrigin")
            ; ("INBDT",         "Start-up date")
            ; ("STATTEXT",      "Status")
            ; ("STSM_EQUI",     "Status Profile")
            ; ("USTW_EQUI",     "Status of an object")
            ; ("USWO_EQUI",     "Status without status number")
            ; ("LAGER_EQI",     "Storage location")
            ; ("ANL2_EILO",     "Sub-number")
            ; ("HEQU_EEQZ",     "Superord. Equipment")
            ; ("TIDN_EEQZ",     "Technical IdentNo.")
            ; ("GEWEI",         "Unit of weight")
            ; ("DATA_EEQZ",     "Valid From")
            ; ("DATB_EEQZ",     "Valid To")
            ; ("DATBI_EIL",     "Valid To")
            ; ("ELIEF_EQI",     "Vendor")
            ; ("PROI_EILO",     "WBS Element")
            ; ("PROIDI",        "WBS element origin")
            ; ("CGWLEN_EQ",     "Warranty End Date(C)")
            ; ("VGWLEN_EQ",     "Warranty end date(V)")
            ; ("CWAGET_EQ",     "Warranty inheritance possible(C)")
            ; ("VWAGET_EQ",     "Warranty inheritance possible(V)")
            ; ("ARBP_EILO",     "Work center")
            ]
    
    // ************************************************************************
    // classequi fields

    let private classequiColumnAcronyms : Map<string,string> = 
        Map.ofList <|
            [ ("CLASS",         "Class")
            ; ("CLASSTYPE",     "Class Type")
            ; ("CLINT",         "Internal class no")
            ; ("CLSTATUS1",     "Status")
            ; ("EQUI",          "Equipment")            
            ]

            
    // ************************************************************************
    // valuaequi fields

    let private valuaequiColumnAcronyms : Map<string,string> = 
        Map.ofList <|            
            [ ("ATAW1",         "Alternative unit")
            ; ("ATAWE",         "Alternative unit")
            ; ("ATAUT",         "Author")
            ; ("CHARID",        "Characteristic ID")
            ; ("ATNAM",         "Characteristic Name")
            ; ("ATWRT",         "Characteristic Value")
            ; ("CLASSTYPE",     "Class Type")
            ; ("ATCOD",         "Code")
            ; ("ATVGLART",      "Comp. type")
            ; ("TEXTBEZ",       "Description")
            ; ("EQUI",          "Equipment")
            ; ("ATZIS",         "Instance counter")
            ; ("VALCNT",        "Int counter values")
            ; ("ATIMB",         "Internal char no.")
            ; ("ATSRT",         "Position")
            ; ("ATFLV",         "Value from")
            ; ("ATFLB",         "Value to")
            ]


    // ************************************************************************
    // Decode function

    let decodeAcronym (entityType : EntityType) 
                      (acronym : string) : string option = 
        match entityType with
        | FuncLoc -> Map.tryFind acronym funclocColumnAcronyms  
        | ClassFloc -> Map.tryFind acronym classflocColumnAcronyms  
        | ValuaFloc -> Map.tryFind acronym valuaflocColumnAcronyms
        | Equi -> Map.tryFind acronym equiColumnAcronyms
        | ClassEqui -> Map.tryFind acronym classequiColumnAcronyms
        | ValuaEqui -> Map.tryFind acronym valuaequiColumnAcronyms

    let getHeaderDescriptions (entityType : EntityType) 
                            (headers : HeaderRow) : HeaderRow = 
        headers.Columns 
            |> List.map (decodeAcronym entityType >> Option.defaultValue "")
            |> List.toArray
            |> HeaderRow

