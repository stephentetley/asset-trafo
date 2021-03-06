﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.PrologFacts


module XsbAibDbExport =

    open FSharp.Data
    open FactX
    open FactX.FactWriter
    
    
    open AssetSync.Base.FactsCommon
    open AssetSync.Base.DbExportSchema



    /// All facts have the same format (arity 5) but different
    /// predicate names 
    let makeFact (categoryName : string) 
                    (factName : string) 
                    (row : AibFlocRow) : Predicate option = 
        if row.Category= categoryName then
            predicate factName 
                        [ quotedAtom row.Reference
                        ; quotedAtom row.AssetName
                        ; quotedAtom row.AssetType
                        ; quotedAtom row.CommonName
                        ; quotedAtom row.ParentRef
                        ] |> Some
        else None

    let makePredicate (categoryName : string) 
                        (makePred : AibFlocRow -> Predicate) 
                        (row : AibFlocRow) : Predicate option = 
        if row.Category= categoryName then
            makePred row |> Some
        else None

    let generateInstallationFacts (rows : AibFlocRow list) 
                                  (outputFile : string) : unit = 
        let instPred (row : AibFlocRow) = 
            predicate "aib_floc_l1_l2_installation" 
                [ quotedAtom row.Reference
                ; quotedAtom row.AssetName
                ; quotedAtom row.AssetCode
                ]
        writeFactsWithHeaderComment outputFile
            <| generatePredicates (makePredicate "INSTALLATION" instPred) rows


    let generateProcessGroupFacts (rows : AibFlocRow list) 
                                  (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
            <| generatePredicates (makeFact "PROCESS GROUP" "aib_floc_l3_process_group") rows
                   

    let generateProcessFacts (rows : AibFlocRow list) 
                             (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
             <| generatePredicates (makeFact "PROCESS" "aib_floc_l4_process") rows

    
    let generatePlantFacts (rows : AibFlocRow list) 
                           (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
             <| generatePredicates (makeFact "PLANT" "aib_floc_l5_plant") rows

    let generatePlantItemFacts (rows : AibFlocRow list) 
                               (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
             <| generatePredicates (makeFact "PLANT ITEM" "aib_floc_l6_plant_item") rows



    /// This is not a 'floc' fact but a (hopefully) speedier 
    /// lookup table.
    let generateCategoryFacts (rows : AibFlocRow list) 
                              (outputFile : string) : unit = 
        let makePred1 (row : AibFlocRow) : Predicate option = 
            predicate "aib_asset_category"
                [ quotedAtom row.Reference
                ; quotedAtom row.Category
                ] |> Some
        writeFactsWithHeaderComment outputFile
             <| generatePredicates makePred1 rows


    
    let makeEquipmentFact (row : AibEquipmentRow) : Predicate option = 
        predicate "aib_equipment" 
                    [ quotedAtom row.Reference
                    ; quotedAtom row.AssetType
                    ; quotedAtom row.AssetName
                    ; quotedAtom row.CommonName
                    ; quotedAtom row.ParentRef
                    ] |> Some

    let generateEquipmentFacts (rows : AibEquipmentRow list) 
                               (outputFile : string) : unit =            
            writeFactsWithHeaderComment outputFile
                <| generatePredicates makeEquipmentFact rows


