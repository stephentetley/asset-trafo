// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module PopulateChangesDb =

    open SLSqlite.Utils
    open SLSqlite.SqliteDb

    open AssetSync.ChangesReport.ImportSchema


    // Helper
    let insertStatement (tableName : string) (columnNames : string []) (values : string []) : string =
        let columns = String.concat ", " columnNames
        let vals = String.concat ", " values
        sprintf "INSERT INTO %s (%s)\nVALUES (%s);" tableName columns vals
        
    // ************************************************************************
    // Table: asset_change

    let assetChangeInsert (row : AssetChangeRow) : string option =
        let columns =
            [| "change_request_id"
            ; "request_status"
            ; "change_request_type"
            ; "asset_reference"
            ; "ai_asset_name"
            ; "ai_common_name"
            ; "ai_installed_from_date"
            ; "ai_manufacturer"
            ; "ai_model"
            ; "ai_hierarchy_key"
            ; "ai_asset_status"
            ; "ai_location_reference"
            ; "aide_asset_name"
            ; "aide_common_name"
            ; "aide_installed_from_date"
            ; "aide_manufacturer"
            ; "aide_model"
            ; "aide_hierarchy_key"
            ; "aide_asset_status"
            ; "aide_location_reference"
            ; "change_request_time"
            |]
        insertStatement "asset_change"
                        columns
                        [| row.ChangeRequestId          |> fmtBigInt
                         ; row.RequestStatus            |> fmtText
                         ; row.ChangeRequestType        |> fmtText
                         ; row.AssetReference           |> fmtText
                         ; row.AiAssetName              |> fmtText
                         ; row.AiCommonName             |> fmtText
                         ; row.AiInstalledFromDate      |> toIso8601String |> fmtText
                         ; row.AiManufacturer           |> fmtText
                         ; row.AiModel                  |> fmtText
                         ; row.AiHierarchyKey           |> fmtText
                         ; row.AiAssetStatus            |> fmtText
                         ; row.AiLocationReference      |> fmtText
                         ; row.AideAssetName            |> fmtText
                         ; row.AideCommonName           |> fmtText
                         ; row.AideInstalledFromDate    |> toIso8601String |> fmtText
                         ; row.AideManufacturer         |> fmtText
                         ; row.AideModel                |> fmtText
                         ; row.AideHierarchyKey         |> fmtText
                         ; row.AideAssetStatus          |> fmtText
                         ; row.AideLocationReference    |> fmtText
                         ; row.ChangeRequestTime        |> toIso8601String |> fmtText
                        |] |> Some

    let insertAssetChangeRow (row : AssetChangeRow) : SqliteDb<unit> = 
        match assetChangeInsert row with
        | Some statement -> 
            printfn "%s" statement
            executeNonQuery statement |>> ignore
        | None -> mreturn ()

    let insertAsssetChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAssetChangeExport csvPath)
            let rows = table.Rows |> Seq.toList
            return! withTransaction <| forMz rows insertAssetChangeRow
        }

    // ************************************************************************
    // Table: attribute_change

    let attributeChangeInsert (row : AttributeChangeRow) : string option =
        let columns =
            [| "attribute_change_id"
            ; "change_request_id"
            ; "request_status"
            ; "reference"
            ; "asset_name"
            ; "attribute_name"
            ; "ai_value"
            ; "ai_lookup_value"
            ; "ai_lookup_code"
            ; "aide_value"
            ; "aide_lookup_value"
            ; "aide_lookup_code"
            ; "change_request_time"
            |]
        insertStatement "attribute_change"
                        columns
                        [| row.AideAssetAttributeValueId    |> fmtBigInt
                        ; row.ChangeRequestId               |> fmtBigInt
                        ; row.RequestStatus                 |> fmtText
                        ; row.Reference                     |> fmtText
                        ; row.AiAssetName                   |> fmtText
                        ; row.AttributeName                 |> fmtText
                        ; row.AiValue                       |> Option.defaultValue "" |> fmtText
                        ; row.AiLookupValue                 |> Option.defaultValue "" |> fmtText
                        ; row.AideLookupCode                |> Option.defaultValue (-1L) |> fmtBigInt
                        ; row.AideValue                     |> Option.defaultValue "" |> fmtText
                        ; row.AideLookupValue               |> Option.defaultValue "" |> fmtText
                        ; row.AideLookupCode                |> Option.defaultValue (-1L) |> fmtBigInt
                        ; row.ChangeRequestTime             |> toIso8601String |> fmtText 
                        |] |> Some

    let insertAttributeChangeRow (row : AttributeChangeRow) : SqliteDb<unit> = 
        match attributeChangeInsert row with
        | Some statement -> 
            printfn "%s" statement
            executeNonQuery statement |>> ignore
        | None -> mreturn ()


    let insertAttributeChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAttributeChangeExport csvPath)
            let rows = table.Rows |> Seq.toList
            return! withTransaction <| forMz rows insertAttributeChangeRow
        }

    
    // ************************************************************************
    // Table: attribute_change

    let repeatedAttributeChangeInsert (row : RepeatedAttributeChangeRow) : string option =
        let columns =
            [| "repeated_attribute_change_id"
            ; "change_request_id"
            ; "request_status"
            ; "reference"
            ; "asset_name"
            ; "attribute_name"
            ; "attribute_set_name"
            ; "ai_value"
            ; "ai_lookup_value"
            ; "ai_lookup_code"
            ; "aide_value"
            ; "aide_lookup_value"
            ; "aide_lookup_code"
            ; "change_request_time"
            |]
        insertStatement "repeated_attribute_change"
                        columns
                        [| row.AideAssetAttributeValueId    |> fmtBigInt
                        ; row.ChangeRequestId               |> fmtBigInt
                        ; row.RequestStatus                 |> fmtText
                        ; row.Reference                     |> fmtText
                        ; row.AiAssetName                   |> fmtText
                        ; row.AttributeName                 |> fmtText
                        ; row.AttributeSetName              |> fmtText
                        ; row.AiValue                       |> Option.defaultValue "" |> fmtText
                        ; row.AiLookupValue                 |> Option.defaultValue "" |> fmtText
                        ; row.AideLookupCode                |> Option.defaultValue (-1L) |> fmtBigInt
                        ; row.AideValue                     |> Option.defaultValue "" |> fmtText
                        ; row.AideLookupValue               |> Option.defaultValue "" |> fmtText
                        ; row.AideLookupCode                |> Option.defaultValue (-1L) |> fmtBigInt
                        ; row.ChangeRequestTime             |> toIso8601String |> fmtText 
                        |] |> Some

    let insertRepeatedAttributeChangeRow (row : RepeatedAttributeChangeRow) : SqliteDb<unit> = 
        match repeatedAttributeChangeInsert row with
        | Some statement -> 
            printfn "%s" statement
            executeNonQuery statement |>> ignore
        | None -> mreturn ()


    let insertRepeatedAttributeChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readRepeatedAttributeChangeExport csvPath)
            let rows = table.Rows |> Seq.toList
            return! withTransaction <| forMz rows insertRepeatedAttributeChangeRow
        }

    
    



    // Structure relationship changes?
