// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AspFacts


module PlantMapping =

    open FSharp.Data
    open FactX
    open FactX.FactWriter

    open AssetTrafo.AspFacts.Common

    // ********** DATA SETUP **********

    [<Literal>]
    let PlantTableSchema = 
        "Reference(string),HKey(string),\
         AssetName(string),AssetType(string),\
         Category(string),CommonName(string)"    

    [<Literal>]
    let PlantTableSample = 
         "SAI0101,2OLDWW,NO 1 STARTER,MOTOR STARTER,PLANT ITEM,COMMON NAME/WITH/SEPARATORS"
     

    type PlantTable = 
        CsvProvider< Schema = PlantTableSchema
                   , Sample = PlantTableSample
                   , HasHeaders = true >

    type PlantRow = PlantTable.Row

    let getRows (cvsPath : string) : PlantRow list = 
        let table = PlantTable.Load(uri = cvsPath)
        table.Rows |> Seq.toList

    let commonNameGetInstallation (commonName : string) : string = 
        let elts = commonName.Split([| '/' |])
        String.concat "/" [ elts.[0]; elts.[1]]

    /// TODO - this only works if the we know it is a process group at .[2]
    /// This is not generally the case and we should decode the hkey to check 
    /// whether it has a mask.
    let commonNameGetProcessGroup (commonName : string) (hkey : string) : string = 
        let elts = commonName.Split([| '/' |])
        elts.[2]

    let processGroupFact (row : PlantRow) : Predicate option = 
        match row.Category with
        | "PROCESS GROUP" ->
            let pgName = commonNameGetProcessGroup row.CommonName row.HKey
            predicate "processGroup" 
                        [ stringTerm row.Reference
                        ; stringTerm row.AssetType
                        ; stringTerm (commonNameGetInstallation row.CommonName)
                        ; stringTerm pgName
                        ]
                |> Some
        | _ -> None

    let generateProcessGroupFacts (rows : PlantRow list) 
                                     (outputFile : string) : unit =            
            writeFactsWithHeaderComment outputFile
                <| generatePredicates processGroupFact rows
                   

    //let eqptRelation (row : RuleRow) : Predicate option = 
    //    match row.RelationshipType with
    //    | "EQPTRULES" ->
    //        predicate "eqpt" 
    //                    [ stringTerm row.AssetType1
    //                    ; stringTerm row.AssetType2 
    //                    ]
    //            |> Some
    //    | _ -> None


    //let generateFunLocFacts (ruleTableCsvSourceFile : string) 
    //                        (outputFile : string) : unit = 
    //    let rows = getRows ruleTableCsvSourceFile
    //    generatePredicates funcLocFact rows
    //        |> writeFactsWithHeaderComment outputFile 
         

    //let generateEquipmentFacts (ruleTableCsvSourceFile : string) 
    //                            (outputFile : string) : unit = 
    //    let rows = getRows ruleTableCsvSourceFile
    //    generatePredicates eqptRelation rows 
    //        |> writeFactsWithHeaderComment outputFile 