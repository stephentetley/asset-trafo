// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module PopulateChangesDb =

    open System.Data.SQLite

    open SLSqlite.Core

    open AssetSync.ChangesReport.ImportSchema


    // FSharp.Data.CsvReader appears to read csv "NULL" 
    // as the literal string "NULL".
    // Making the column value type `string option` doesn't
    // seem to change this so provide an explicit work around
    let stringParamAux (source : string) : SQLiteParameter =
        match source with
        | null | "NULL" | "null" -> nullParam ()
        | _ -> stringParam source



    // ************************************************************************
    // Table: change_request

    let changeRequestInsert (row : ChangeRequestsRow) : IndexedCommand option =
        let sql =
            "INSERT INTO change_request \
            (change_request_id, \
            change_request_time, \
            change_request_type, \
            change_request_status, \
            change_request_comments) \
            VALUES \
            (?,?,?,  ?,?)"
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.ChangeRequestId)
                |> addParam (dateTimeParam row.ChangeRequestTime)
                |> addParam (stringParam row.ChangeRequestType)
                |> addParam (stringParam row.ChangeRequestStatus)
                |> addParam (stringParam row.ChangeRequestComments)
        cmd |> Some

    let insertChangeRequestRow (row : ChangeRequestsRow) : SqliteDb<unit> = 
        match changeRequestInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()

    let insertChangeRequestRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readChangeRequestsExport csvPath)
            return! withTransaction <| sforMz table.Rows insertChangeRequestRow
        }

        
    // ************************************************************************
    // Table: change_request_asset

    let assetChangeInsert (row : ChangeReqAssetsRow) : IndexedCommand option =
        let sql =
            "INSERT INTO change_request_asset \
            (aide_asset_id, \
            change_request_id, \
            ai_asset_reference, \
            aide_asset_reference, \
            ai_asset_name, \
            ai_common_name, \
            ai_installed_from_date, \
            ai_manufacturer, \
            ai_model, \
            ai_hierarchy_key, \
            ai_asset_status, \
            ai_location_reference, \
            ai_asset_deleted, \
            aide_asset_name, \
            aide_common_name, \
            aide_installed_from_date, \
            aide_manufacturer, \
            aide_model, \
            aide_hierarchy_key, \
            aide_asset_status, \
            aide_location_reference, \
            aide_asset_deleted) \
            VALUES \
            (?,?,?,  ?,?,?,  ?,?,?,  ?,?,?,  ?,?,?,  ?,?,?, ?,?,?,  ?)"
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.AideAssetId)
                |> addParam (int64Param row.ChangeRequestId)
                |> addParam (stringParam row.AiAssetReference)
                |> addParam (stringParam row.AideAssetReference)
                |> addParam (stringParam row.AiAssetName)
                |> addParam (stringParam row.AiCommonName)
                |> addParam (dateTimeParam row.AiInstalledFromDate)
                |> addParam (optionNull stringParam row.AiManufacturer)
                |> addParam (optionNull stringParam row.AiModel)
                |> addParam (stringParam row.AiHierarchyKey)
                |> addParam (stringParam row.AiAssetStatus)
                |> addParam (stringParam row.AiLocationReference)
                |> addParam (int32Param row.AiAssetDeleted)
                |> addParam (stringParam row.AideAssetName)
                |> addParam (stringParam row.AideCommonName)
                |> addParam (dateTimeParam row.AideInstalledFromDate)
                |> addParam (optionNull stringParam row.AideManufacturer)
                |> addParam (optionNull stringParam row.AideModel)
                |> addParam (stringParam row.AideHierarchyKey)
                |> addParam (stringParam row.AideAssetStatus)
                |> addParam (stringParam row.AideLocationReference)
                |> addParam (int32Param row.AideAssetDeleted)
        cmd |> Some

    let insertAssetChangeRow (row : ChangeReqAssetsRow) : SqliteDb<unit> = 
        match assetChangeInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()

    let insertAsssetChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readChangeReqAssetsExport csvPath)
            return! withTransaction <| sforMz table.Rows insertAssetChangeRow
        }

    // ************************************************************************
    // Table: change_request_attribute

    let attributeChangeInsert (row : ChangeReqAttributesRow) : IndexedCommand option =
        let sql =
            "INSERT INTO change_request_attribute \
            (aide_asset_attr_value_id, \
            change_request_id, \
            asset_reference, \
            asset_name, \
            asset_common_name, \
            attribute_name, \
            ai_value, \
            ai_lookup_value, \
            aide_value, \
            aide_lookup_value) \
            VALUES (?,?,?,  ?,?,?,  ?,?,?, ?)"
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.AssetAttrValueId)
                |> addParam (int64Param row.ChangeRequestId)
                |> addParam (stringParam row.AssetReference)
                |> addParam (stringParam row.AssetName)
                |> addParam (stringParam row.AssetCommonName)
                |> addParam (stringParam row.AttributeName)
                |> addParam (optionNull stringParamAux row.AiValue)
                |> addParam (optionNull stringParamAux row.AiLookupValue)
                |> addParam (optionNull stringParamAux row.AideValue)
                |> addParam (optionNull stringParamAux row.AideLookupValue)
        cmd |> Some

    let insertAttributeChangeRow (row : ChangeReqAttributesRow) : SqliteDb<unit> = 
        match attributeChangeInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()


    let insertAttributeChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readChangeReqAttributesExport csvPath)
            return! withTransaction <| sforMz table.Rows insertAttributeChangeRow
        }

    
    // ************************************************************************
    // Table: change_request_repeated_attribute

    let repeatedAttributeChangeInsert (row : ChangeReqRepeatedAttributesRow) : IndexedCommand option =
        let sql =        
            "INSERT INTO change_request_repeated_attribute \
            (aide_asset_attr_repeating_value_id, \
            change_request_id, \
            asset_reference, \
            asset_name, \
            asset_common_name, \
            attribute_name, \
            attribute_set_name, \
            ai_value, \
            ai_lookup_value, \
            aide_value, \
            aide_lookup_value) \
            VALUES (?,?,?,  ?,?,?,  ?,?,?,  ?,?)"
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.AssetAttrRepeatingValueId)
                |> addParam (int64Param row.ChangeRequestId)
                |> addParam (stringParam row.AssetReference)
                |> addParam (stringParam row.AssetName)
                |> addParam (stringParam row.AssetCommonName)
                |> addParam (stringParam row.AttributeName)
                |> addParam (optionNull stringParamAux row.AttributeSetName)
                |> addParam (optionNull stringParamAux row.AiValue)
                |> addParam (optionNull stringParamAux row.AiLookupValue)
                |> addParam (optionNull stringParamAux row.AideValue)
                |> addParam (optionNull stringParamAux row.AideLookupValue)
        cmd |> Some

    let insertRepeatedAttributeChangeRow (row : ChangeReqRepeatedAttributesRow) : SqliteDb<unit> = 
        match repeatedAttributeChangeInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()


    let insertRepeatedAttributeChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readChangeReqRepeatedAttributesExport csvPath)
            return! withTransaction <| sforMz table.Rows insertRepeatedAttributeChangeRow
        }

    
    



    // Structure relationship changes?
