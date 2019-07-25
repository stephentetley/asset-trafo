// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AspFacts


module Level234FlocMapping =

    open FSharp.Interop.Excel

    open FactX
    open FactX.FactWriter

    open AssetTrafo.AspFacts.Common

    // ********** DATA SETUP **********
    type MappingTable = 
        ExcelFile< @"G:\work\Projects\asset_sync\AI2_FLOC_Asset_Hierarchy_Rules_V3_FRAGMENT.xlsx",
                    SheetName = "Mapping!",
                    ForceString = true >
    
    type MappingRow = MappingTable.Row


    let getMappingRows (xlsxPath : string) : MappingRow list = 
        let isBlank (row : MappingRow) = 
            try 
                // First column / cell should be an int
                match row.InstAssetTypeID with
                | null | "" -> true
                | _ -> false
            with
            | _ -> true
        (new MappingTable(filename = xlsxPath)).Data
            |> Seq.filter (not << isBlank)
            |> Seq.toList


    let checkInput (input : string) : string option = 
        match input with
        | null | "NULL" -> None
        | _ -> input.Trim() |> Some

    // ************************************************************************
    // Processes and Process Groups (Aib)
    
    let private aibPrcg (row : MappingRow) : Predicate option = 
        let make1 = fun name -> predicate "aib_process_group" [ stringTerm name ]
        checkInput row.PrcgAssetTypeDescription
            |> Option.map make1
    
    let private aibPrc (row : MappingRow) : Predicate option = 
        let make1 = fun name -> predicate "aib_process" [ stringTerm name ]
        checkInput row.PrcAssetTypeDescription
            |> Option.map make1

    

    let generateProcProcGroupFacts (mappingRows : MappingRow list)
                                    (outputFile : string) : unit =  
        writeFactsWithHeaderComment outputFile
            <| factWriter { 
                    do! generatePredicates aibPrcg mappingRows
                    do! generatePredicates aibPrc mappingRows
                    return ()
                }

    // ************************************************************************
    // Description Lookups (code to description) (S4)

    let level2Lookup (row : MappingRow) : Predicate option = 
        let make1 = 
            fun code desc -> predicate "level2_function_description" [ stringTerm code; stringTerm desc ]
        match (checkInput row.``L2 FLOC Code/Object Code``, 
                checkInput row.``Function (L2 FLOC Description)``) with
        | Some code, Some desc -> make1 code desc |> Some
        | _, _ -> None

    let level3Lookup (row : MappingRow) : Predicate option = 
        let make1 = 
            fun code desc -> predicate "level3_prcg_description" [ stringTerm code; stringTerm desc ]
        match (checkInput row.``L3 FLOC Code/Object Code``, 
                checkInput row.``Process Group (L3 FLOC Description)``) with
        | Some code, Some desc -> make1 code desc |> Some
        | _, _ -> None

    let level4Lookup (row : MappingRow) : Predicate option = 
        let make1 = 
            fun code desc -> predicate "level4_prc_description" [ stringTerm code; stringTerm desc ]
        match (checkInput row.``L4 FLOC Code/Object Code``, 
                checkInput row.``Process (L4 FLOC Description)``) with
        | Some code, Some desc -> make1 code desc |> Some
        | _, _ -> None

    let generateDescriptionLookupFacts (mappingRows : MappingRow list)
                                        (outputFile : string) : unit =  
        writeFactsWithHeaderComment outputFile
            <| factWriter { 
                    do! generatePredicates level2Lookup mappingRows
                    do! generatePredicates level3Lookup mappingRows
                    do! generatePredicates level4Lookup mappingRows
                    return ()
                }

    // ************************************************************************
    // Code mapping


    let level234Mapping (row:MappingRow) : Predicate option = 
        let quoted1 (item : string) : Term = 
            checkInput item
                |> Option.defaultValue ""
                |> stringTerm
        predicate "level234_mapping" 
                    [ quoted1 row.InstAssetTypeCode
                    ; quoted1 row.PrcgAssetTypeDescription
                    ; quoted1 row.PrcAssetTypeDescription
                    ; quoted1 row.``L2 FLOC Code/Object Code``
                    ; quoted1 row.``L3 FLOC Code/Object Code``
                    ; quoted1 row.``L4 FLOC Code/Object Code``
                    ]
            |> Some

    let generateLevel234Mapping (mappingRows : MappingRow list)
                                (outputFile : string) : unit =  
        writeFactsWithHeaderComment outputFile
            <| generatePredicates level234Mapping mappingRows