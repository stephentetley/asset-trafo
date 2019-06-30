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


type SiteMappingTable = 
    ExcelFile< @"G:\work\Projects\asset_sync\Lvl1_2FlocMapping.xlsx"
             , HasHeaders = true
             , ForceString = true >

type SiteMappingRow = SiteMappingTable.Row

let readSiteMappingRows () : seq<SiteMappingRow> = 
    let table = new SiteMappingTable()
    let isBlank (row : SiteMappingRow) = 
        match row.AI2_InstallationReference with 
        | null | "" -> true
        | _ -> false
    table.Data |> Seq.filter (not << isBlank)


let jsonString (input:string) : JsonValue = 
    match input with
    | null | "" -> JsonValue.String "NULL"
    | _ -> JsonValue.String input


// "flocInfo": { "type":"CSO", "s4Name":"Tollerton Bridge CSO", "level1Code":"TOL05", "level2Code":"WTN" }
let flocInfoRecord (row : SiteMappingRow) : JsonValue = 
    JsonValue.Record 
        [|  ("type",            jsonString row.AI2_InstallationAssetType)
        ;   ("s4Name",          jsonString row.``S/4 Hana Floc Description``)
        ;   ("level1Code",      jsonString row.``S/4 Hana Floc Lvl1_Code``)        
        ;   ("level2Code",      jsonString row.``S/4 Hana Floc Lvl2_Code``)
        |]

let rowToJsonValue (row : SiteMappingRow) : JsonValue =
    JsonValue.Record 
        [|  ("sai", jsonString row.AI2_InstallationReference)
        ;   ("flocInfo",  flocInfoRecord row) 
        |]


let rowsToJson (rows : seq<SiteMappingRow>) : JsonValue =
    rows 
        |> Seq.map rowToJsonValue
        |> Seq.toArray
        |> JsonValue.Array

let generate (outputPath:string) : unit = 
    let json = readSiteMappingRows () |> rowsToJson
    use sw = new StreamWriter (path = outputPath)
    json.WriteTo(sw, JsonSaveOptions.None)

let main () = 
    generate @"G:\work\Projects\asset_sync\output\aib_to_s4_level1_2.json"