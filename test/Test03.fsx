// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.1.1\lib\netstandard2.0"
#r "FSharp.Data.dll"
open FSharp.Data


#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190616\lib\netstandard2.0"
#r "SLFormat.dll"


#I @"C:\Users\stephen\.nuget\packages\tikzdoc\1.0.0-alpha-20190711\lib\netstandard2.0"
#r "TikZDoc.dll"


#load "..\src\AssetTrafo\Base\TreeDiff.fs"
#load "..\src\AssetTrafo\Aib\StructureRelationsSimple.fs"
open AssetTrafo.Aib.TreeDiff
open AssetTrafo.Aib.StructureRelationsSimple

let outputDirectory () = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", "data\output\latex")

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

let diff (sourcePath : string) (targetPath : string) : Result<Diff<string> list, string> = 
    let source = loadStructureRelationships sourcePath
    let target = loadStructureRelationships targetPath
    match source, target with
    | Some src, Some dest -> treeDiff src dest |> Ok

    | None, _ -> Error "Could not read source"
    | _, None -> Error "Could not read target"

let draw (sourcePath : string) : unit = 
    match loadStructureRelationships sourcePath with
    | None -> printfn "draw - Loading failed"
    | Some ans -> printfn "%s" <| drawAibTree ans


let drawTikZ (sourcePath : string) (outputName : string) : unit = 
    match loadStructureRelationships sourcePath with
    | None -> printfn "draw - Loading failed"
    | Some ans -> 
        let outputPath =  outputDirectory ()
        let doc = renderAibTreeTikZ ans
        doc.SaveToPS(outputPath, System.IO.Path.ChangeExtension(outputName, "ps"))
    

let demo02 () = 
    let source = localFile @"data\aldw_kids_relations.csv"
    let target = localFile @"data\aide_aldw_kids_relations.csv"
    diff source target


let demo03 () = 
    let source = localFile @"data\some_cso_kids_relations.csv"
    let target = localFile @"data\aide_some_cso_kids_relations.csv"
    draw source
    let ans = diff source target 
    match ans with 
    | Ok ds -> 
        printfn "%i edit steps" ds.Length
        List.iter (printfn "%O") ds
    | Error msg -> printfn "Fail: %s" msg
    draw target
    


let test04 () = 
    let source = localFile @"data\some_cso_kids_relations.csv"
    drawTikZ source "forest1-ps.ps"


