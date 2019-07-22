// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.0.1\lib\netstandard2.0"
#r @"FSharp.Data.dll"
open FSharp.Data


#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat"

#I @"C:\Users\stephen\.nuget\packages\factx\1.0.0-alpha-20190721\lib\netstandard2.0"
#r "FactX"
open FactX
open FactX.FactWriter
open FactX.Skeletons



let localFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, "..\..", relativePath)


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

let funcLocRelation (row : RuleRow) : Predicate option = 
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

/// TODO - FactWriter should have a tellPredicates function that removes duplicates 
/// (and maybe sorts)
let main () = 
    let rows = getRows @"G:\work\Projects\asset_sync\rules\ai_rule_table_simple.csv"
    List.choose funcLocRelation rows
        |> List.distinct
        |> mapMz tellPredicate
        |> runFactWriter 160 (localFile @"clingo\facts\funcloc.lp") 

    List.choose eqptRelation rows
        |> List.distinct
        |> mapMz tellPredicate
        |> runFactWriter 160 (localFile @"clingo\facts\eqpt.lp") 
         
