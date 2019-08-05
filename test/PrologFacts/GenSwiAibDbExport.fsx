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
#load "..\..\src\AssetTrafo\PrologFacts\AibDbExportCommon.fs"
#load "..\..\src\AssetTrafo\PrologFacts\SwiAibDbExport.fs"
open AssetTrafo.PrologFacts.AibDbExportCommon
open AssetTrafo.PrologFacts.SwiAibDbExport


let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\output\swi-prolog", relativePath)




let genEquipmentFacts (csvSource : string) : unit =  
    let rows = getEquipmentRows csvSource |> Seq.toList
    generateEquipmentFacts "aib_inst_facts1" rows (outputFile "aib_inst_facts1.pl")

let main () = 
    genEquipmentFacts @"G:\work\Projects\asset_sync\rules\aib_equipment_extract1.csv"
    printfn "Done." 
