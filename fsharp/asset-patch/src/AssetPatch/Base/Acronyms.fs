﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Acronyms = 

    open AssetPatch.Base.Syntax

    let private funclocColumnAcronyms : Map<string,string> = 
        Map.ofList <|
            [ ("ABCKZFLOC", "ABC Indicator")
            ; ("ABCKZI", "ABC Indic. Origin")
            ; ("ANSWT", "Acquisition Value")
            ; ("ANSDT", "Acquisition date")
            ; ("DEACT", "Active Indicator Status Indicator")
            ; ("ZADRNR", "Address")
            ; ("ZADRNRI", "Address origin")
            ; ("ANLN1_FL", "Asset")
            ; ("ANLNRI", "Asset no. origin")
            ; ("EINZLI", "AuthGroup origin")
            ; ("BEGRUI", "AuthGroup origin")
            ; ("BEGRU", "AuthorizGroup")
            ; ("CGWLDT_FL", "Begin Guarantee(C)")
            ; ("VGWLDT_FL", "Begin Guarantee(V)")
            ; ("GSBE_FLOC", "Business Area")
            ; ("GSBERI", "Business area orig.")
            ; ("RBNR_FLOC", "Catalog Profile")
            ; ("BUKRSI", "CompCode origin")
            ; ("BUKRSFLOC", "Company Code")
            ; ("SUBMTI", "ConstructType origin")
            ; ("BAUMM", "Contrstuction month")
            ; ("SUBMTIFLO", "Construction type Material")
            ; ("BAUJJ", "Construction year")
            ; ("KOKRSI", "ControlArea origin")
            ; ("KOKR_FLOC", "Controlling Area")
            ; ("KOST_FLOC", "Cost Center")
            ; ("KOSTLI", "Cost center origin")
            ; ("HERLD", "Country of manufact.")
            ; ("WAERS", "Currency")
            ; ("LVORM", "Deletion Indicator")
            ; ("TXTMI", "Description (medium text) ")
            ; ("USTA_FLOC", "Display lines for user status")
            ; ("VTWEG", "Distribution Channel")
            ; ("SPART", "Division")
            ; ("IEQUII", "EquipInst. Origin")
            ; ("EQUI_FLOC", "Equipment")
            ; ("FLTYP", "FuncLocCategory")
            ; ("FUNCLOC", "Function Location")
            ; ("BRGEW", "Gross weight")
            ; ("CGAERB_FL", "Indicator: Pass on warranty(C)")
            ; ("VGAERB_FL", "Indicator: Pass on warranty(V)")
            ; ("IEQUI", "Installation allowed")
            ; ("INVNR", "Inventory number")
            ; ("LIZNR", "License number")
            ; ("STOR_FLOC", "Location")
            ; ("STORTI", "Location origin")
            ; ("GEWRKFLOC", "Main work center")
            ; ("INGR_FLOC", "Maint Planner Group")
            ; ("RBNR_I", "MaintCatalog origin")
            ; ("INGRPI", "MaintPlGroup origin")
            ; ("SWERKI", "MaintPlant origin")
            ; ("SWERK_FL", "Maintenance Plat")
            ; ("SERGE", "ManufSerialNumber")
            ; ("MAPAR", "ManufactPartNo.")
            ; ("HERST", "Manufacturer")
            ; ("FLOC_REF", "Masked Functional Location")
            ; ("CMGANR_FL", "Master Warranty(C)")
            ; ("VMGANR_FL", "Master Warranty(V)")
            ; ("TYPBZ", "Model number")
            ; ("OBJIDFLOC", "Object ID")
            ; ("OBJTYFLOC", "Object Type")
            ; ("EQART", "Object Type")
            ; ("JOBJN_FL", "Object Number")
            ; ("PPSIDI", "PP WrkCenter origin")
            ; ("PLNT_FLOC", "Planning  Plant")
            ; ("BEBER_FL", "Plant Section")
            ; ("BEBERI", "Plant Section Origin")
            ; ("WERGWFLOC", "Plant for WorkCenter")
            ; ("IWERKI", "Plant plant origin")
            ; ("POSNR", "Position in object")
            ; ("TRPNR1", "Reference FL fo CR Processing")
            ; ("TRPNR", "Reference Location")
            ; ("MSGRP", "Room")
            ; ("MSGRPI", "Room number origin")
            ; ("VKORG", "Sales Organization")
            ; ("VKGRP", "Sales group")
            ; ("VKBUR", "Sales office")
            ; ("VKORGI", "SalesOrg origin")
            ; ("AUFN_FLOC", "Settlement Order")
            ; ("AUFNRI", "SettlementOrderOrigin")
            ; ("IFLOT_SRT", "Shift Report Type")
            ; ("EINZL", "Single installation")
            ; ("GROES", "Size/dimension")
            ; ("EQFNR", "Sort Field")
            ; ("DAUF_FLOC", "Standing Order")
            ; ("DAUFNI", "StandingOrderOrigin")
            ; ("INBDT", "Start-up date")
            ; ("STATTEXT", "Status")
            ; ("STSM_FLOC", "Status Profile")
            ; ("USTW_FLOC", "Status of an object")
            ; ("USWO_FLOC", "Status without status number")
            ; ("TPLKZ_FLC", "Structure indicator")
            ; ("ANLA_FL", "Sub-number")
            ; ("TPLMA1", "Superior FL for CR Processing")
            ; ("TPLMA", "Superior FunctLoc")
            ; ("GEWEI", "Unit of weight")
            ; ("STTXU", "User Status")
            ; ("DATBI_FLO", "Valid To")
            ; ("PROI_FLOC", "WBS Element")
            ; ("PROIDI", "WBS element origin")
            ; ("CGWLEN_FL", "Warranty End Date(C)")
            ; ("VGWLEN_FL", "Warranty End Date(V)")
            ; ("CWAGET_FL", "Warranty inheritance possible(C)")
            ; ("VWAGET_FL", "Warranty inheritance possible(V)")
            ; ("ARBPLFLOC", "Work center")
            ; ("LGWIDI", "Work center origin")
            ]

    let private classflocColumnAcronyms : Map<string,string> = 
        Map.ofList <|
            [ ("CLASS", "Class")
            ; ("CLASSTYPE", "Class Type")
            ; ("CLINT", "Internal class no.")
            ; ("CLSTATUS1", "Status")
            ; ("FUNCLOC", "Functional Location")
            ]
    
    let private valuaflocColumnAcronyms : Map<string,string> = 
        Map.ofList <|
            [ ("ATAW1", "Alternative unit")
            ; ("ATAWE", "Alternative unit")
            ; ("ATAUT", "Author")
            ; ("CHARID", "Characteristic ID")
            ; ("ATNAM", "Characteristic Name")
            ; ("ATWRT", "Characteristic Value")
            ; ("CLASSTYPE", "Class Type")
            ; ("ATCOD", "Code")
            ; ("ATVGLART", "Comp. type")
            ; ("TEXTBEZ", "Description")
            ; ("FUNCLOC", "Function Location")
            ; ("ATZIS", "Instance counter")
            ; ("VALCNT", "Int count values")
            ; ("ATIMB", "Internal char no.")
            ; ("ATSRT", "Position")
            ; ("ATFLV", "Value from ")
            ; ("ATFLB", "Value to")
            ]    

    let decodeAcronym (entityType : EntityType) 
                      (acronym : string) : string option = 
        match entityType with
        | FuncLoc -> Map.tryFind acronym funclocColumnAcronyms  
        | ClassFloc -> Map.tryFind acronym classflocColumnAcronyms  
        | ValuaFloc -> Map.tryFind acronym valuaflocColumnAcronyms
        | Equi | ClassEqui | ValuaEqui -> None
