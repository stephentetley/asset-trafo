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


#load "..\..\src\AssetTrafo\Base\FactsCommon.fs"
#load "..\..\src\AssetTrafo\Base\DbExportSchema.fs"
#load "..\..\src\AssetTrafo\PrologFacts\SwiAibDbExport.fs"
open AssetTrafo.Base.DbExportSchema
open AssetTrafo.PrologFacts.SwiAibDbExport


let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\..\output\swi-prolog", relativePath)



let main () = 
    let patt = "^ABBEY LANE HULL.*(SPS|CSO)"
    let flocCsv = @"G:\work\Projects\asset_sync\rules\aib_floc_extract4.csv"
    let equipCsv = @"G:\work\Projects\asset_sync\rules\aib_equipment_extract1.csv"
    let flocRows = getFlocRows flocCsv |> Seq.toList
    let equipRows = getEquipmentRows equipCsv |> Seq.toList
    generateExportFacts patt "aib_inst_facts1" flocRows equipRows (outputFile "aib_inst_facts1.pl")
    printfn "Done." 
