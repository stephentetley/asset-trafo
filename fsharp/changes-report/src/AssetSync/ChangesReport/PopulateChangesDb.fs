// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module PopulateChangesDb =

    open System.Data.SQLite

    open SLSqlite.Utils
    open SLSqlite.SqliteDb

    open AssetSync.ChangesReport.ImportSchema


        
    // ************************************************************************
    // Table: asset_change

    let assetChangeInsert (row : AssetChangeRow) : SQLiteCommand option =
        let sql =
            "INSERT INTO asset_change \
            (change_request_id, \
            request_status, \
            change_request_type, \
            asset_reference, \
            ai_asset_name, \
            ai_common_name, \
            ai_installed_from_date, \
            ai_manufacturer, \
            ai_model, \
            ai_hierarchy_key, \
            ai_asset_status, \
            ai_location_reference, \
            aide_asset_name, \
            aide_common_name, \
            aide_installed_from_date, \
            aide_manufacturer, \
            aide_model, \
            aide_hierarchy_key, \
            aide_asset_status, \
            aide_location_reference, \
            change_request_time) \
            VALUES \
            (?,?,?, ?,?,?, ?,?,?, ?,?,?, ?,?,?, ?,?,?, ?,?,?)"
        let cmd = new SQLiteCommand(commandText = sql)
        cmd.Parameters.Add(box row.ChangeRequestId) |> ignore
        cmd.Parameters.Add(box row.RequestStatus) |> ignore
        cmd.Parameters.Add(box row.ChangeRequestType) |> ignore
        cmd.Parameters.Add(box row.AssetReference) |> ignore
        cmd.Parameters.Add(box row.AiAssetName) |> ignore
        cmd.Parameters.Add(box row.AiCommonName) |> ignore
        cmd.Parameters.Add(box row.AiInstalledFromDate) |> ignore
        cmd.Parameters.Add(box row.AiManufacturer) |> ignore
        cmd.Parameters.Add(box row.AiModel) |> ignore
        cmd.Parameters.Add(box row.AiHierarchyKey) |> ignore
        cmd.Parameters.Add(box row.AiAssetStatus) |> ignore
        cmd.Parameters.Add(box row.AiLocationReference) |> ignore
        cmd.Parameters.Add(box row.AideAssetName) |> ignore
        cmd.Parameters.Add(box row.AideCommonName) |> ignore
        cmd.Parameters.Add(box row.AideInstalledFromDate) |> ignore
        cmd.Parameters.Add(box row.AideManufacturer) |> ignore
        cmd.Parameters.Add(box row.AideModel) |> ignore
        cmd.Parameters.Add(box row.AideHierarchyKey) |> ignore
        cmd.Parameters.Add(box row.AideAssetStatus) |> ignore
        cmd.Parameters.Add(box row.AideLocationReference) |> ignore
        cmd.Parameters.Add(box row.ChangeRequestTime) |> ignore
        cmd |> Some

    let insertAssetChangeRow (row : AssetChangeRow) : SqliteDb<unit> = 
        match assetChangeInsert row with
        | Some statement -> 
            executeNonQuery statement |>> ignore
        | None -> mreturn ()

    let insertAsssetChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAssetChangeExport csvPath)
            return! withTransaction <| sforMz table.Rows insertAssetChangeRow
        }

    // ************************************************************************
    // Table: attribute_change

    let attributeChangeInsert (row : AttributeChangeRow) : SQLiteCommand option =
        let sql =
            "INSERT INTO attribute_change \
            (attribute_change_id, \
            change_request_id, \
            request_status, \
            reference, \
            asset_name, \
            attribute_name, \
            ai_value, \
            ai_lookup_value, \
            ai_lookup_code, \
            aide_value, \
            aide_lookup_value, \
            aide_lookup_code, \
            change_request_time) \
            VALUES (?,?,?, ?,?,?, ?,?,?, ?,?,?, ?)"
        let cmd = new SQLiteCommand(commandText = sql)
        cmd.Parameters.Add(box row.AideAssetAttributeValueId) |> ignore
        cmd.Parameters.Add(box row.ChangeRequestId) |> ignore
        cmd.Parameters.Add(box row.RequestStatus) |> ignore
        cmd.Parameters.Add(box row.Reference) |> ignore
        cmd.Parameters.Add(box row.AiAssetName) |> ignore
        cmd.Parameters.Add(box row.AttributeName) |> ignore
        cmd.Parameters.Add(box row.AiValue) |> ignore
        cmd.Parameters.Add(box row.AiLookupValue) |> ignore
        cmd.Parameters.Add(box row.AideLookupCode) |> ignore
        cmd.Parameters.Add(box row.AideValue) |> ignore
        cmd.Parameters.Add(box row.AideLookupValue) |> ignore
        cmd.Parameters.Add(box row.AideLookupCode) |> ignore
        cmd.Parameters.Add(box row.ChangeRequestTime) |> ignore
        cmd |> Some

    let insertAttributeChangeRow (row : AttributeChangeRow) : SqliteDb<unit> = 
        match attributeChangeInsert row with
        | Some statement -> 
            executeNonQuery statement |>> ignore
        | None -> mreturn ()


    let insertAttributeChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAttributeChangeExport csvPath)
            return! withTransaction <| sforMz table.Rows insertAttributeChangeRow
        }

    
    // ************************************************************************
    // Table: attribute_change

    let repeatedAttributeChangeInsert (row : RepeatedAttributeChangeRow) : SQLiteCommand option =
        let sql =        
            "INSERT INTO repeated_attribute_change \
            (repeated_attribute_change_id, \
            change_request_id, \
            request_status, \
            reference, \
            asset_name, \
            attribute_name, \
            attribute_set_name, \
            ai_value, \
            ai_lookup_value, \
            ai_lookup_code, \
            aide_value, \
            aide_lookup_value, \
            aide_lookup_code, \
            change_request_time) \
            VALUES (?,?,?, ?,?,?, ?,?,?, ?,?,?, ?,?)"
        let cmd = new SQLiteCommand(commandText = sql)
        cmd.Parameters.Add(row.AideAssetAttributeValueId) |> ignore
        cmd.Parameters.Add(row.ChangeRequestId) |> ignore
        cmd.Parameters.Add(row.RequestStatus) |> ignore
        cmd.Parameters.Add(row.Reference) |> ignore
        cmd.Parameters.Add(row.AiAssetName) |> ignore
        cmd.Parameters.Add(row.AttributeName) |> ignore
        cmd.Parameters.Add(row.AttributeSetName) |> ignore
        cmd.Parameters.Add(row.AiValue) |> ignore
        cmd.Parameters.Add(row.AiLookupValue) |> ignore
        cmd.Parameters.Add(row.AideLookupCode) |> ignore 
        cmd.Parameters.Add(row.AideValue) |> ignore
        cmd.Parameters.Add(row.AideLookupValue) |> ignore
        cmd.Parameters.Add(row.AideLookupCode) |> ignore
        cmd.Parameters.Add(row.ChangeRequestTime) |> ignore
        cmd |> Some

    let insertRepeatedAttributeChangeRow (row : RepeatedAttributeChangeRow) : SqliteDb<unit> = 
        match repeatedAttributeChangeInsert row with
        | Some statement -> 
            executeNonQuery statement |>> ignore
        | None -> mreturn ()


    let insertRepeatedAttributeChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readRepeatedAttributeChangeExport csvPath)
            return! withTransaction <| sforMz table.Rows insertRepeatedAttributeChangeRow
        }

    
    



    // Structure relationship changes?
