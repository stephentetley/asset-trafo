// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.XsbFacts


module PlantMapping =

    open FSharp.Data
    open FactX
    open FactX.FactWriter
    
    open AssetTrafo.Aib.HKey
    open AssetTrafo.Base.FactsCommon

    // ********** DATA SETUP **********

    [<Literal>]
    let PlantTableSchema = 
        "Reference(string),HKey(string),\
         AssetName(string),AssetType(string),\
         Category(string),CommonName(string),\
         ParentRef(string)"    

    [<Literal>]
    let PlantTableSample = 
         "SAI0101,2OLDWW,NO 1 STARTER,MOTOR STARTER,PLANT ITEM,COMMON NAME/WITH/SEPARATORS,SAI0100"
     

    type PlantTable = 
        CsvProvider< Schema = PlantTableSchema
                   , Sample = PlantTableSample
                   , HasHeaders = true >

    type PlantRow = PlantTable.Row

    let getRows (cvsPath : string) : PlantRow list = 
        let table = PlantTable.Load(uri = cvsPath)
        table.Rows |> Seq.toList



    /// All facts have the same format (arity 5) but different
    /// predicate names 
    let makeFact (categoryName : string) 
                    (factName : string) 
                    (row : PlantRow) : Predicate option = 
        if row.Category= categoryName then
            predicate factName 
                        [ quotedAtom row.Reference
                        ; quotedAtom row.AssetType
                        ; quotedAtom row.AssetName
                        ; quotedAtom row.CommonName
                        ; quotedAtom row.ParentRef
                        ] |> Some
        else None

    let generateProcessGroupFacts (rows : PlantRow list) 
                                     (outputFile : string) : unit =            
            writeFactsWithHeaderComment outputFile
                <| generatePredicates (makeFact "PROCESS GROUP" "aib_process_group") rows
                   

    let generateProcessFacts (rows : PlantRow list) 
                                (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
             <| generatePredicates (makeFact "PROCESS" "aib_process") rows

    
    let generatePlantFacts (rows : PlantRow list) 
                                (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
             <| generatePredicates (makeFact "PLANT" "aib_plant") rows

    let generatePlantItemFacts (rows : PlantRow list) 
                                (outputFile : string) : unit =            
        writeFactsWithHeaderComment outputFile
             <| generatePredicates (makeFact "PLANT ITEM" "aib_plant_item") rows