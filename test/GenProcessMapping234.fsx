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

// { 
// , "s4":  { "level2Floc":"WWT", "level3Floc":"TER", "level4Floc":"PHC" }
// }

let jsonString (input:string) : JsonValue = 
    match input with
    | null | "" -> JsonValue.String "NULL"
    | _ -> JsonValue.String input

// "aib": { "instType":"STW", "progGroupDesc":"TERTIARY TREATMENT", "procDesc":"LIME DOSING"}
let aibJsonRecord (row : ProcessMappingRow) : JsonValue = 
    JsonValue.Record 
        [|  ("instType",        jsonString row.InstAssetTypeCode)
        ;   ("progGroupDesc",   jsonString row.PrcgAssetTypeDescription)
        ;   ("procDesc",        jsonString row.PrcAssetTypeDescription)
        |]

// "s4":  { "level2Floc":"WWT", "level3Floc":"TER", "level4Floc":"PHC" }
let s4JsonRecord (row : ProcessMappingRow) : JsonValue = 
    JsonValue.Record 
        [|  ("level2Floc",      jsonString row.``L2 FLOC Code/Object Code``)
        ;   ("level3Floc",      jsonString row.``L3 FLOC Code/Object Code``)
        ;   ("level4Floc",      jsonString row.``L4 FLOC Code/Object Code``)
        |]

let rowToJsonValue (row : ProcessMappingRow) : JsonValue =
    JsonValue.Record 
        [|  ("aib", aibJsonRecord row);  ("s4",  s4JsonRecord row) |]


let rowsToJson (rows : seq<ProcessMappingRow>) : JsonValue =
    rows 
        |> Seq.map rowToJsonValue
        |> Seq.toArray
        |> JsonValue.Array

let generate (outputPath:string) : unit = 
    let json = readProcessMappingRows () |> rowsToJson
    use sw = new StreamWriter (path = outputPath)
    json.WriteTo(sw, JsonSaveOptions.None)

let main () = 
    generate @"G:\work\Projects\asset_sync\output\aib_to_s4_level2_3_4.json"