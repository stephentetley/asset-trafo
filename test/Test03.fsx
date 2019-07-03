// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.1.1\lib\netstandard2.0"
#r "FSharp.Data.dll"
open FSharp.Data

#load "..\src\AssetTrafo\Aib\StructureRelationsSimple.fs"
open AssetTrafo.Aib.StructureRelationsSimple

let localFile (pathSuffix : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", pathSuffix)

let convertToJson (sourcePath : string ) (outPath : string) : unit = 
    match readRelationshipsExport sourcePath |> buildStructureTree with
    | None -> failwith "read error"
    | Some tree -> 
        use sw = new StreamWriter(outPath)
        let json = tree.WriteJson ()        
        json.WriteTo(sw, JsonSaveOptions.None)

let demo01 () = 
    convertToJson (localFile @"data\aldw_kids_relations.csv") 
                    (localFile @"data\output\aldw_kids_relations.json") 

    convertToJson (localFile @"data\aide_aldw_kids_relations.csv") 
                    (localFile @"data\output\aide_aldw_kids_relations.json") 


