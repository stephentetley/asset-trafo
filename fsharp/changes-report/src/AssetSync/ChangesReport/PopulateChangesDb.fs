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
            (:p1, :p2, :p3, :p4, :p5, :p6, :p7, :p8, :p9, :p10, :p11, :p12, :p13, :p14, :p15, :p16, :p17, :p18, :p19, :p20, :p21)"
        let cmd = new SQLiteCommand(commandText = sql)
        // cmd.Parameters.Add(box row.ChangeRequestId) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p1", value = box row.ChangeRequestId) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p2", value = box row.RequestStatus) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p3", value = box row.ChangeRequestType) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p4", value = box row.AssetReference) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p5", value = box row.AiAssetName) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p6", value = box row.AiCommonName) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p7", value = box row.AiInstalledFromDate) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p8", value = box row.AiManufacturer) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p9", value = box row.AiModel) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p10", value = box row.AiHierarchyKey) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p11", value = box row.AiAssetStatus) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p12", value = box row.AiLocationReference) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p13", value = box row.AideAssetName) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p14", value = box row.AideCommonName) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p15", value = box row.AideInstalledFromDate) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p16", value = box row.AideManufacturer) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p17", value = box row.AideModel) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p18", value = box row.AideHierarchyKey) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p19", value = box row.AideAssetStatus) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p20", value = box row.AideLocationReference) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p21", value = box row.ChangeRequestTime) |> ignore
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
            VALUES (:p1, :p2, :p3, :p4, :p5, :p6, :p7, :p8, :p9, :p10, :p11, :p12, :p13)"
        let cmd = new SQLiteCommand(commandText = sql)
        cmd.Parameters.AddWithValue(parameterName = "p1", value = box row.AideAssetAttributeValueId) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p2", value = box row.ChangeRequestId) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p3", value = box row.RequestStatus) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p4", value = box row.Reference) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p5", value = box row.AiAssetName) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p6", value = box row.AttributeName) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p7", value = box row.AiValue) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p8", value = box row.AiLookupValue) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p9", value = box row.AideLookupCode) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p10", value = box row.AideValue) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p11", value = box row.AideLookupValue) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p12", value = box row.AideLookupCode) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p13", value = box row.ChangeRequestTime) |> ignore
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
            VALUES (:p1, :p2, :p3, :p4, :p5, :p6, :p7, :p8, :p9, :p10, :p11, :p12, :p13, :p14)"
        let cmd = new SQLiteCommand(commandText = sql)
        cmd.Parameters.AddWithValue(parameterName = "p1", value = box row.AideAssetAttributeValueId) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p2", value = box row.ChangeRequestId) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p3", value = box row.RequestStatus) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p4", value = box row.Reference) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p5", value = box row.AiAssetName) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p6", value = box row.AttributeName) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p7", value = box row.AttributeSetName) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p8", value = box row.AiValue) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p9", value = box row.AiLookupValue) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p10", value = box row.AideLookupCode) |> ignore 
        cmd.Parameters.AddWithValue(parameterName = "p11", value = box row.AideValue) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p12", value = box row.AideLookupValue) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p13", value = box row.AideLookupCode) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "p14", value = box row.ChangeRequestTime) |> ignore
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
