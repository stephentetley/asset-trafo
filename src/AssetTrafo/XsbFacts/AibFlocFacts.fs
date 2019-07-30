// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.XsbFacts


module AibFlocFacts =

    open FSharp.Data
    open FactX
    open FactX.FactWriter
    
    
    open AssetTrafo.Base.FactsCommon

    // ********** DATA SETUP **********

    [<Literal>]
    let FlocTableSchema = 
        "Reference(string),HKey(string),\
         AssetName(string),AssetType(string),\
         AssetCode(string), Category(string),\
         CommonName(string),ParentRef(string)"    

    [<Literal>]
    let FlocTableSample = 
         "SAI0101,2OLDWW,NO 1 STARTER,MOTOR STARTER,MOTR,PLANT ITEM,COMMON NAME/WITH/SEPARATORS,SAI0100"
     

    type FlocTable = 
        CsvProvider< Schema = FlocTableSchema
                   , Sample = FlocTableSample
                   , HasHeaders = true >

    type FlocRow = FlocTable.Row

    let getFlocRows (cvsPath : string) : FlocRow list = 
        let table = FlocTable.Load(uri = cvsPath)
        table.Rows |> Seq.toList



    /// All facts have the same format (arity 5) but different
    /// predicate names 
    let makeFact (categoryName : string) 
                    (factName : string) 
                    (row : FlocRow) : Predicate option = 
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
                        (makePred : FlocRow -> Predicate) 
                        (row : FlocRow) : Predicate option = 
        if row.Category= categoryName then
            makePred row |> Some
        else None

    let generateInstallationFacts (rows : FlocRow list) 
                                     (outputFile : string) : unit = 
        let instPred (row : FlocRow) = 
            predicate "aib_floc_l1_l2_installation" 
                [ quotedAtom row.Reference
                ; quotedAtom row.AssetName
                ; quotedAtom row.AssetCode
                ]
        writeFactsWithHeaderComment outputFile
            <| generatePredicates (makePredicate "INSTALLATION" instPred) rows


    let generateProcessGroupFacts (rows : FlocRow list) 
                                     (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
            <| generatePredicates (makeFact "PROCESS GROUP" "aib_floc_l3_process_group") rows
                   

    let generateProcessFacts (rows : FlocRow list) 
                                (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
             <| generatePredicates (makeFact "PROCESS" "aib_floc_l4_process") rows

    
    let generatePlantFacts (rows : FlocRow list) 
                                (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
             <| generatePredicates (makeFact "PLANT" "aib_floc_l5_plant") rows

    let generatePlantItemFacts (rows : FlocRow list) 
                                (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
             <| generatePredicates (makeFact "PLANT ITEM" "aib_floc_l6_plant_item") rows



    /// This is not a 'floc' fact but a (hopefully) speedier 
    /// lookup table.
    let generateCategoryFacts (rows : FlocRow list) 
                                (outputFile : string) : unit = 
        let makePred1 (row : FlocRow) : Predicate option = 
            predicate "aib_asset_category"
                [ quotedAtom row.Reference
                ; quotedAtom row.Category
                ] |> Some
        writeFactsWithHeaderComment outputFile
             <| generatePredicates makePred1 rows