// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.1.1\lib\netstandard2.0"
#r "FSharp.Data.dll"
open FSharp.Data

#load "..\src\AssetTrafo\Base\TreeDiff.fs"
#load "..\src\AssetTrafo\Aib\StructureRelationsSimple.fs"
open AssetTrafo.Aib.TreeDiff
open AssetTrafo.Aib.StructureRelationsSimple

let localFile (pathSuffix : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", pathSuffix)

let convertToJson (sourcePath : string ) (outPath : string) : unit = 
    match readRelationshipsExport sourcePath |> buildStructureTree with
    | None -> failwith "read error"
    | Some tree -> 
        use sw = new StreamWriter(outPath)
        let json = aibTreeToJson tree     
        json.WriteTo(sw, JsonSaveOptions.None)

let demo01 () = 
    convertToJson (localFile @"data\aldw_kids_relations.csv") 
                    (localFile @"data\output\aldw_kids_relations.json") 

    convertToJson (localFile @"data\aide_aldw_kids_relations.csv") 
                    (localFile @"data\output\aide_aldw_kids_relations.json") 

let demo02 () = 
    let source = loadStructureRelationships (localFile @"data\aldw_kids_relations.csv") 
    let target = loadStructureRelationships (localFile @"data\aide_aldw_kids_relations.csv") 
    match source, target with
    | Some src, Some dest -> treeDiff src dest
    | None, _ -> failwith "Could not read source"
    | _, None -> failwith "Could not read target"