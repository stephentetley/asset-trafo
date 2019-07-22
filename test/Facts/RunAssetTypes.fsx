﻿// Copyright (c) Stephen Tetley 2019

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

#load "..\..\src\AssetTrafo\AspFacts\AssetTypes.fs"
open AssetTrafo.AspFacts.AssetTypes

let clingoDirectory () = 
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, @"..\..\clingo")


let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\clingo\facts", relativePath)


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
        [ "facts/funcloc.lp"
        ; "facts/base_asset_type.lp" 
        ; "queries/funcloc_query.lp"
        ]
    clingo workingDir (Some 0) [] files