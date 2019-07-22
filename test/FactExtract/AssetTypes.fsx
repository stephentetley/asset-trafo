// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.0.1\lib\netstandard2.0"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\portable-net45+win8+wp8+wpa81"
#r "FParsec"
#r "FParsecCS"
open FParsec

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat"

#I @"C:\Users\stephen\.nuget\packages\factx\1.0.0-alpha-20190721\lib\netstandard2.0"
#r "FactX"
open FactX
open FactX.FactWriter
open FactX.Skeletons

#I @"C:\Users\stephen\.nuget\packages\slpotassco\1.0.0-alpha-20190721\lib\netstandard2.0"
#r "SLPotassco"
open SLPotassco.Potassco.Invoke



let clingoDirectory () = 
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, @"..\..\clingo")


let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\clingo\facts", relativePath)


// ********** DATA SETUP **********


[<Literal>]
let TypeCatTableSchema = 
    "SuperCategory(string),AssetType(string),\
     AssetTypeDescription(string),AssetTypeCode(string),\
     Category(string option),Group(string option),\
     SubGroup(string option)"

[<Literal>]
let TypeCatTableSample = 
     "SUPER CAT,NAME,DESCRIPTION,ABC123,CAT,GROUP,SUBGROUP"
     

type TypeCatTable = 
    CsvProvider< Schema = TypeCatTableSchema
               , Sample = TypeCatTableSample
               , HasHeaders = true >

type TypeCatRow = TypeCatTable.Row

let getRows (cvsPath : string) : TypeCatRow list = 
    let table = TypeCatTable.Load(uri = cvsPath)
    table.Rows |> Seq.toList


let baseAssetType (row : TypeCatRow) : Predicate option = 
    match row.SuperCategory, row.Category with
    | _, None | _, Some "NULL" -> None
    | "BASE ASSET TYPE", Some category ->
        predicate "base_asset_type" 
                    [ stringTerm row.AssetType
                    ; stringTerm category
                    ]
            |> Some
    | _ -> None




let equipmentCategory (row : TypeCatRow) : Predicate option =
    match row.SuperCategory, row.Category with
    | _, None | _, Some "NULL" -> None
    | "EQUIPMENT CATEGORY", Some category ->
        predicate "equipment_category" 
                    [ stringTerm row.AssetType
                    ; stringTerm category
                    ]
            |> Some
    | _ -> None

let equipmentGroup (row : TypeCatRow) : Predicate option =
    match row.SuperCategory, row.Group with
    | _, None | _, Some "NULL" -> None
    | "EQUIPMENT CATEGORY", Some group ->
        predicate "equipment_group" 
                    [ stringTerm row.AssetType
                    ; stringTerm group
                    ]
            |> Some
    | _ -> None


// Don't make SubGroup - it is not telling us anything we don't know
let equipmentSubGroup (row : TypeCatRow) : Predicate option =
    match row.SuperCategory, row.SubGroup with
    | _, None | _, Some "NULL" -> None
    | "EQUIPMENT CATEGORY", Some subgroup ->
        predicate "equipment_subgroup" 
                    [ stringTerm row.AssetType
                    ; stringTerm subgroup
                    ]
            |> Some
    | _ -> None

let main () = 
    let rows = getRows @"G:\work\Projects\asset_sync\rules\asset_types_groups_categories.csv"
    
    let genPredicates (makePred : TypeCatRow -> Predicate option) : FactWriter<unit> =
        List.choose makePred rows
            |> List.distinct
            |> mapMz tellPredicate

    runFactWriter 160 (outputFile @"base_asset_type.lp") 
                        (genPredicates baseAssetType)


    runFactWriter 160 (outputFile @"equipment_cats_groups.lp") 
                        (genPredicates equipmentCategory 
                            >>. genPredicates equipmentGroup)

let query () =  
    let workingDir = clingoDirectory ()
    printfn "Working Directory: %s" workingDir
    let files = 
        [ @"facts/funcloc.lp"
        ; @"facts/base_asset_type.lp" 
        ; @"queries/funcloc_query.lp"
        ]
    clingo workingDir (Some 0) [] files