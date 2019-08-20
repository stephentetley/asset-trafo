// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module PopulateChangesDb =

    open System.Data.SQLite

    open SLSqlite.Core

    open AssetSync.ChangesReport.ImportSchema


        
    // ************************************************************************
    // Table: asset_change

    let assetChangeInsert (row : AssetChangeRow) : IndexedCommand option =
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
            (?,?,?,  ?,?,?,  ?,?,?,  ?,?,?,  ?,?,?,  ?,?,?, ?,?,?)"
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.ChangeRequestId)
                |> addParam (stringParam row.RequestStatus)
                |> addParam (stringParam row.ChangeRequestType)
                |> addParam (stringParam row.AssetReference)
                |> addParam (stringParam row.AiAssetName)
                |> addParam (stringParam row.AiCommonName)
                |> addParam (dateTimeParam row.AiInstalledFromDate)
                |> addParam (stringParam row.AiManufacturer)
                |> addParam (stringParam row.AiModel)
                |> addParam (stringParam row.AiHierarchyKey)
                |> addParam (stringParam row.AiAssetStatus)
                |> addParam (stringParam row.AiLocationReference)
                |> addParam (stringParam row.AideAssetName)
                |> addParam (stringParam row.AideCommonName)
                |> addParam (dateTimeParam row.AideInstalledFromDate)
                |> addParam (stringParam row.AideManufacturer)
                |> addParam (stringParam row.AideModel)
                |> addParam (stringParam row.AideHierarchyKey)
                |> addParam (stringParam row.AideAssetStatus)
                |> addParam (stringParam row.AideLocationReference)
                |> addParam (dateTimeParam row.ChangeRequestTime)
        cmd |> Some

    let insertAssetChangeRow (row : AssetChangeRow) : SqliteDb<unit> = 
        match assetChangeInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()

    let insertAsssetChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAssetChangeExport csvPath)
            return! withTransaction <| sforMz table.Rows insertAssetChangeRow
        }

    // ************************************************************************
    // Table: attribute_change

    let attributeChangeInsert (row : AttributeChangeRow) : IndexedCommand option =
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
            VALUES (?,?,?,  ?,?,?,  ?,?,?,  ?,?,?, ?)"
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.AideAssetAttributeValueId)
                |> addParam (int64Param row.ChangeRequestId)
                |> addParam (stringParam row.RequestStatus)
                |> addParam (stringParam row.Reference)
                |> addParam (stringParam row.AiAssetName)
                |> addParam (stringParam row.AttributeName)
                |> addParam (optionNull stringParam row.AiValue)
                |> addParam (optionNull stringParam row.AiLookupValue)
                |> addParam (optionNull int64Param row.AideLookupCode)
                |> addParam (optionNull stringParam row.AideValue)
                |> addParam (optionNull stringParam row.AideLookupValue)
                |> addParam (optionNull int64Param  row.AideLookupCode)
                |> addParam (dateTimeParam row.ChangeRequestTime)
        cmd |> Some

    let insertAttributeChangeRow (row : AttributeChangeRow) : SqliteDb<unit> = 
        match attributeChangeInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()


    let insertAttributeChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAttributeChangeExport csvPath)
            return! withTransaction <| sforMz table.Rows insertAttributeChangeRow
        }

    
    // ************************************************************************
    // Table: attribute_change

    let repeatedAttributeChangeInsert (row : RepeatedAttributeChangeRow) : IndexedCommand option =
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
            VALUES (?,?,?,  ?,?,?,  ?,?,?,  ?,?,?,  ?,?)"
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.AideAssetAttributeValueId)
                |> addParam (int64Param row.ChangeRequestId)
                |> addParam (stringParam row.RequestStatus)
                |> addParam (stringParam row.Reference)
                |> addParam (stringParam row.AiAssetName)
                |> addParam (stringParam row.AttributeName)
                |> addParam (stringParam row.AttributeSetName)
                |> addParam (optionNull stringParam row.AiValue)
                |> addParam (optionNull stringParam row.AiLookupValue)
                |> addParam (optionNull int64Param row.AideLookupCode) 
                |> addParam (optionNull stringParam row.AideValue)
                |> addParam (optionNull stringParam row.AideLookupValue)
                |> addParam (optionNull int64Param row.AideLookupCode)
                |> addParam (dateTimeParam row.ChangeRequestTime)
        cmd |> Some

    let insertRepeatedAttributeChangeRow (row : RepeatedAttributeChangeRow) : SqliteDb<unit> = 
        match repeatedAttributeChangeInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()


    let insertRepeatedAttributeChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readRepeatedAttributeChangeExport csvPath)
            return! withTransaction <| sforMz table.Rows insertRepeatedAttributeChangeRow
        }

    
    



    // Structure relationship changes?
