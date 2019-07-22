// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AspFacts


module AibRuleTable =

    open FSharp.Data
    open FactX
    open FactX.FactWriter

    open AssetTrafo.AspFacts.Common

    // ********** DATA SETUP **********

    [<Literal>]
    let RuleTableSchema = 
        "RuleId(int64),RelationshipType(string),\
         RelationshipTypeDesciption(string),AssetType1(string),AssetType2(string)"    

    [<Literal>]
    let RuleTableSample = 
         "1,DESIGNATOR,Description,TYPE 1,TYPE 2"
     

    type RuleTable = 
        CsvProvider< Schema = RuleTableSchema
                   , Sample = RuleTableSample
                   , HasHeaders = true >

    type RuleRow = RuleTable.Row

    let getRows (cvsPath : string) : RuleRow list = 
        let table = RuleTable.Load(uri = cvsPath)
        table.Rows |> Seq.toList

    let funcLocFact (row : RuleRow) : Predicate option = 
        match row.RelationshipType with
        | "FUNCLOCRULES" ->
            predicate "funcloc" 
                        [ stringTerm row.AssetType1
                        ; stringTerm row.AssetType2 
                        ]
                |> Some
        | _ -> None



    let eqptRelation (row : RuleRow) : Predicate option = 
        match row.RelationshipType with
        | "EQPTRULES" ->
            predicate "eqpt" 
                        [ stringTerm row.AssetType1
                        ; stringTerm row.AssetType2 
                        ]
                |> Some
        | _ -> None


    let generateFunLocFacts (ruleTableCsvSourceFile : string) 
                            (outputFile : string) : unit = 
        let rows = getRows ruleTableCsvSourceFile
        generatePredicates funcLocFact rows
            |> runFactWriter 160 outputFile
         

    let generateEquipmentFacts (ruleTableCsvSourceFile : string) 
                                (outputFile : string) : unit = 
        let rows = getRows ruleTableCsvSourceFile
        generatePredicates eqptRelation rows 
            |> runFactWriter 160 outputFile