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


#load "..\..\src\AssetSync\Base\FactsCommon.fs"
#load "..\..\src\AssetSync\PrologFacts\S4EquipmentExport.fs"
open AssetSync.PrologFacts.S4EquipmentExport


let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\..\output\swi-prolog", relativePath)



let main () = 
    let patt = "^ABBEY LANE HULL.*(SPS|CSO)"
    let equipCsv = @"G:\work\Projects\asset_sync\equipment_migration_s1.csv"
    let equipRows = getEquipmentRows equipCsv 
    swiS4Equipment patt "s4_equipment_facts" equipRows (outputFile "s4_equipment_facts1.pl")
    printfn "Done." 
