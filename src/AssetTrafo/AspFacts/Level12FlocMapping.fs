// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AspFacts


module Level12FlocMapping =

    open FSharp.Interop.Excel

    open FactX
    open FactX.FactWriter

    open AssetTrafo.AspFacts.Common

    // ********** DATA SETUP **********
    type SiteMappingTable = 
        ExcelFile< @"G:\work\Projects\asset_sync\Lvl1_2FlocMapping.xlsx",
                    SheetName = "Mapping!",
                    ForceString = true >
    
    type SiteMappingRow = SiteMappingTable.Row


    let getSiteMappingRows (xlsxPath : string) : SiteMappingRow list = 
        let isBlank (row : SiteMappingRow) = 
            try 
                // First column / cell should be an int
                match row.AI2_InstallationReference with
                | null | "" -> true
                | _ -> false
            with
            | _ -> true
        (new SiteMappingTable(filename = xlsxPath)).Data
            |> Seq.filter (not << isBlank)
            |> Seq.toList


    let checkInput (input : string) : string option = 
        match input with
        | null | "NULL" -> None
        | _ -> input.Trim() |> Some


    // ************************************************************************
    // Aib Common Name
    
    let private aibCommonName (row : SiteMappingRow) : Predicate option = 
        let make1 = 
            fun code name -> predicate "aib_common_name" [ stringTerm code; stringTerm name ]
        match (checkInput row.AI2_InstallationReference, 
                checkInput row.AI2_InstallationCommonName) with
        | Some code, Some name -> make1 code name |> Some
        | _, _ -> None


    

    let generateCommonNameFacts (mappingRows : SiteMappingRow list)
                                (outputFile : string) : unit =  
        generatePredicates aibCommonName mappingRows
            |> writeFactsWithHeaderComment outputFile
            



    // ************************************************************************
    // Description Lookups (code to description) (S4)

    let level2Mapping (row : SiteMappingRow) : Predicate option = 
        let make1 = 
            fun sai floc1 s4Name -> 
                predicate "level1_mapping" [ stringTerm sai
                                           ; stringTerm floc1
                                           ; stringTerm s4Name ]
        match (checkInput row.AI2_InstallationReference, 
                checkInput row.``S/4 Hana Floc Lvl1_Code``, 
                checkInput row.``S/4 Hana Floc Description``) with
        | Some code, Some floc1, Some name -> make1 code floc1 name |> Some
        | _, _, _-> None


    let level3Mapping (row : SiteMappingRow) : Predicate option = 
        let make1 = 
            fun sai floc1 floc2 -> 
                predicate "level2_mapping" [ stringTerm sai
                                           ; stringTerm floc1
                                           ; stringTerm floc2 ]
        match (checkInput row.AI2_InstallationReference, 
                checkInput row.``S/4 Hana Floc Lvl1_Code``, 
                checkInput row.``S/4 Hana Floc Lvl2_Code``) with
        | Some code, Some floc1, Some name -> make1 code floc1 name |> Some
        | _, _, _-> None

    let generateLevel12Mappings (mappingRows : SiteMappingRow list)
                                (outputFile : string) : unit =  
        writeFactsWithHeaderComment outputFile
            <| factWriter { 
                    do! generatePredicates level2Mapping mappingRows
                    do! generatePredicates level3Mapping mappingRows
                    return ()
                }


    // ************************************************************************
    // Aib Common Name
    
    let private aibInstallationType (row : SiteMappingRow) : Predicate option = 
        let make1 = 
            fun code typ -> 
                predicate "aib_installation_type" [ stringTerm code; stringTerm typ ]
        match (checkInput row.AI2_InstallationReference, 
                checkInput row.AI2_InstallationAssetType) with
        | Some code, Some typ -> make1 code typ |> Some
        | _, _ -> None


    

    let generateAibInstallationType (mappingRows : SiteMappingRow list)
                            (outputFile : string) : unit =  
        generatePredicates aibInstallationType mappingRows
            |> writeFactsWithHeaderComment outputFile

