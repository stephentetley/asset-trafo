// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\lib\netstandard2.0"
#r "ExcelProvider.Runtime.dll"

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\typeproviders\fsharp41\netstandard2.0"
#r "ExcelDataReader.DataSet.dll"
#r "ExcelDataReader.dll"
#r "ExcelProvider.DesignTime.dll"
open FSharp.Interop.Excel


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.1.1\lib\netstandard2.0"
#r "FSharp.Data.dll"
#r "FSharp.Data.DesignTime.dll"
open FSharp.Data
open FSharp.Data.JsonExtensions


type ProcessMappingTable = 
    ExcelFile< @"G:\work\Projects\asset_sync\AI2_FLOC_Asset_Hierarchy_Rules_V3_FRAGMENT.xlsx"
             , HasHeaders = true
             , ForceString = true >

type ProcessMappingRow = ProcessMappingTable.Row

let readProcessMappingRows () : seq<ProcessMappingRow> = 
    let table = new ProcessMappingTable()
    let isBlank (row : ProcessMappingRow) = 
        match row.InstAssetTypeCode with 
        | null | "" -> true
        | _ -> false
    table.Data |> Seq.filter (not << isBlank)

let unNull (input:string) : string = 
    match input with
    | null -> ""
    | _ -> input



let jsonString (input:string) : JsonValue = 
    match input with
    | null | "" -> JsonValue.String "NULL"
    | _ -> JsonValue.String input

// "aib": { "instType":"STW", "progGroupDesc":"TERTIARY TREATMENT", "procDesc":"LIME DOSING"}
let dictRecord (name:string, description:string) : JsonValue = 
    JsonValue.Record 
        [|  ("key",         jsonString name)
        ;   ("val",         jsonString description)
        |]



let rowsToJson (extractor : ProcessMappingRow -> string * string) 
               (rows : seq<ProcessMappingRow>) : JsonValue =
    rows 
        |> Seq.map extractor
        |> Seq.distinctBy fst
        |> Seq.map dictRecord
        |> Seq.toArray
        |> JsonValue.Array

let generate (extractor : ProcessMappingRow -> string * string)
             (outputPath:string) = 
    let json = readProcessMappingRows () |> rowsToJson extractor
    use sw = new StreamWriter (path = outputPath)
    json.WriteTo(sw, JsonSaveOptions.None)

let main () =
    let level2Extractor (row:ProcessMappingRow) =
        ( unNull row.``L2 FLOC Code/Object Code``
        , unNull row.``Function (L2 FLOC Description)``)

    let level3Extractor (row:ProcessMappingRow) =
        ( unNull row.``L3 FLOC Code/Object Code``
        , unNull row.``Process Group (L3 FLOC Description)``)

    let level4Extractor (row:ProcessMappingRow) =
        ( unNull row.``L4 FLOC Code/Object Code``
        , unNull row.``Process (L4 FLOC Description)``)

    generate level2Extractor @"G:\work\Projects\asset_sync\output\level2_descriptions.json"
    generate level3Extractor @"G:\work\Projects\asset_sync\output\level3_descriptions.json"
    generate level4Extractor @"G:\work\Projects\asset_sync\output\level4_descriptions.json"