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


#load "..\..\src\AssetTrafo\Aib\HKey.fs"
#load "..\..\src\AssetTrafo\AspFacts\Common.fs"
#load "..\..\src\AssetTrafo\AspFacts\PlantMapping.fs"
open AssetTrafo.AspFacts.PlantMapping


let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\clingo\facts", relativePath)






let main () = 
    let source = @"G:\work\Projects\asset_sync\rules\plant_mapping_extract.csv"
    let rows = getRows source |> Seq.toList
    generateProcessGroupFacts rows (outputFile "aib_map_process_groups.lp")
    generateProcessFacts rows (outputFile "aib_map_processes.lp")


let temp01 () = 
    let source = @"G:\work\Projects\asset_sync\rules\plant_mapping_extract.csv"
    let rows = getRows source
    rows |> Seq.fold (fun ac (row : PlantRow) -> max ac (String.length (row.HKey.Trim()) )) 0

let hkeyIsAnon (start : int) (len : int) (hkey : string) : bool =
    let fragment = hkey.[start .. start + (len - 1)]
    String.forall (fun ch -> ch = 'X') fragment