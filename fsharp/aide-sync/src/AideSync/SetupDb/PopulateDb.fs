// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.SetupDb


module PopulateDb =

    open System.Data.SQLite

    open SLSqlite.Core

    open AideSync.ImportSchema


    // FSharp.Data.CsvReader appears to read csv "NULL" 
    // as the literal string "NULL".
    // Making the column value type `string option` doesn't
    // seem to change this so provide an explicit work around
    let stringParamAux (source : string) : SQLiteParameter =
        match source with
        | null | "NULL" | "null" -> nullParam ()
        | _ -> stringParam source

    // ************************************************************************
    // Table: work_scheme

    let WorkSchemeInsert (row : WorkSchemeRow) : IndexedCommand option =
        let sql =        
            """
            INSERT INTO work_scheme
            (scheme_id, 
            scheme_code, 
            scheme_name, 
            description, 
            solution_provider) 
            VALUES (?,?,?,  ?,?);
            """
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.SchemeId)
                |> addParam (stringParam row.SchemeCode)
                |> addParam (stringParam row.SchemeName)
                |> addParam (stringParam row.Description)
                |> addParam (stringParam row.SolutionProvider)
        cmd |> Some

    let insertWorkSchemeRow (row : WorkSchemeRow) : SqliteDb<unit> = 
        match WorkSchemeInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()


    let insertWorkSchemeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readWorkSchemeExport csvPath)
            return! withTransaction <| sforMz table.Rows insertWorkSchemeRow
        }

    // ************************************************************************
    // Table: ai_asset

    let makeAiAssetInsert (row : AiAssetRow) : IndexedCommand option =
        let sql = 
            """
            INSERT INTO ai_asset
            (ai_asset_id,
            reference,
            asset_common_name,
            asset_name,
            asset_type,
            asset_category)
            VALUES (?,?,?,  ?,?,?);
            """
        let cmd = 
            new IndexedCommand (commandText = sql)
                |> addParam (int64Param row.AssetId)
                |> addParam (stringParam row.Reference)              
                |> addParam (stringParam row.AssetCommonName)
                |> addParam (stringParam row.AssetName)
                |> addParam (stringParam row.AssetType)
                |> addParam (stringParam row.AssetCategory)
        cmd |> Some


    let insertAiAssetRow (row : AiAssetRow) : SqliteDb<unit> = 
        match makeAiAssetInsert row with
        | Some statement -> 
            attempt (executeNonQueryIndexed statement)
                    (fun msg -> throwError (sprintf "AI Bad Row %O\n%s" row msg)) |>> ignore
        | None -> mreturn ()

    let insertAiAssetRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAiAssetExport csvPath)
            return! withTransaction <| sforMz table.Rows insertAiAssetRow
        }


    // ************************************************************************
    // Table: aide_asset

    let makeAideAssetInsert (row : AideAssetRow) : IndexedCommand option =
        let sql = 
            """
            INSERT INTO aide_asset
            (aide_asset_id, 
            change_request_id, 
            asset_id,
            reference, 
            asset_common_name, 
            asset_name,
            asset_type, 
            asset_category)
            VALUES (?,?,?,  ?,?,?, ?,?);
            """
        let cmd = 
            new IndexedCommand (commandText = sql)
                |> addParam (int64Param row.AideAssetId)
                |> addParam (optionNull int64Param row.ChangeRequestId)
                |> addParam (optionNull int64Param row.AssetId)
                |> addParam (stringParam row.Reference)                
                |> addParam (stringParam row.AssetCommonName)
                |> addParam (stringParam row.AssetName)
                |> addParam (stringParam row.AssetType)
                |> addParam (stringParam row.AssetCategory)
        cmd |> Some


    let insertAideAssetRow (row : AideAssetRow) : SqliteDb<unit> = 
        match makeAideAssetInsert row with
        | Some statement -> 
            attempt (executeNonQueryIndexed statement)
                    (fun _msg -> throwError (sprintf "Bad Row %O" row)) |>> ignore
        | None -> mreturn ()

    let insertAideAssetRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAideAssetExport csvPath)
            return! withTransaction <| sforMz table.Rows insertAideAssetRow
        }

    // ************************************************************************
    // Table: change_request

    let changeRequestInsert (row : ChangeRequestRow) : IndexedCommand option =
        let sql =
            """
            INSERT INTO change_request
            (change_request_id, 
            change_request_time, 
            change_request_type, 
            change_request_status, 
            comments) 
            VALUES (?,?,?,  ?,?);
            """
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.ChangeRequestId)
                |> addParam (dateTimeParam row.ChangeRequestTime)
                |> addParam (stringParam row.ChangeRequestType)
                |> addParam (stringParam row.ChangeRequestStatus)
                |> addParam (stringParam row.ChangeRequestComments)
        cmd |> Some

    let insertChangeRequestRow (row : ChangeRequestRow) : SqliteDb<unit> = 
        match changeRequestInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()

    let insertChangeRequestRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readChangeRequestExport csvPath)
            return! withTransaction <| sforMz table.Rows insertChangeRequestRow
        }

        
    // ************************************************************************
    // Table: asset_change

    let assetChangeInsert (row : AssetChangeRow) : IndexedCommand option =
        let sql =
            """
            INSERT INTO asset_change
            (aide_asset_id, 
            ai_asset_id, 
            change_request_id, 
            scheme_id, 
            ai_asset_reference, 
            aide_asset_reference, 
            ai_asset_name, 
            ai_common_name, 
            ai_installed_from_date, 
            ai_manufacturer, 
            ai_model, 
            ai_hierarchy_key, 
            ai_asset_status, 
            ai_location_reference, 
            ai_asset_deleted, 
            aide_asset_name, 
            aide_common_name, 
            aide_installed_from_date, 
            aide_manufacturer, 
            aide_model, 
            aide_hierarchy_key, 
            aide_asset_status, 
            aide_location_reference, 
            aide_asset_deleted) 
            VALUES 
            (?,?,?,  ?,?,?,  ?,?,?,  ?,?,?,  ?,?,?,  ?,?,?, ?,?,?,  ?,?,?);
            """
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.AideAssetId)
                |> addParam (optionNull int64Param row.AiAssetId)
                |> addParam (int64Param row.ChangeRequestId)
                |> addParam (optionNull int64Param row.SchemeId)
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

    let insertAssetChangeRow (row : AssetChangeRow) : SqliteDb<unit> = 
        match assetChangeInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()

    let insertAssetChangeRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAssetChangeExport csvPath)
            return! withTransaction <| sforMz table.Rows insertAssetChangeRow
        }

    // ************************************************************************
    // Table: asset_attribute_change

    let attributeChangeInsert (row : AttributeChangeRow) : IndexedCommand option =
        let sql =
            """
            INSERT INTO asset_attribute_change
            (aide_asset_attr_value_id,
            change_request_id, 
            ai_asset_id,
            aide_asset_id,
            asset_reference, 
            asset_name, 
            asset_common_name, 
            attribute_name, 
            ai_value, 
            ai_lookup_value, 
            aide_value, 
            aide_lookup_value) 
            VALUES (?,?,?,  ?,?,?,  ?,?,?, ?,?,?);
            """
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.AssetAttrValueId)
                |> addParam (int64Param row.ChangeRequestId)
                |> addParam (int64Param row.AiAssetId)
                |> addParam (int64Param row.AideAssetId)
                |> addParam (stringParam row.AssetReference)
                |> addParam (stringParam row.AssetName)
                |> addParam (stringParam row.AssetCommonName)
                |> addParam (stringParam row.AttributeName)
                |> addParam (optionNull stringParamAux row.AiValue)
                |> addParam (optionNull stringParamAux row.AiLookupValue)
                |> addParam (optionNull stringParamAux row.AideValue)
                |> addParam (optionNull stringParamAux row.AideLookupValue)
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
    // Table: asset_repeated_attribute_change

    let repeatedAttributeChangeInsert (row : RepeatedAttributeChangeRow) : IndexedCommand option =
        let sql =        
            """
            INSERT INTO asset_repeated_attribute_change 
            (aide_asset_attr_repeating_value_id, 
            change_request_id, 
            ai_asset_id,
            aide_asset_id,
            asset_reference, 
            asset_name, 
            asset_common_name, 
            attribute_name, 
            attribute_set_name, 
            ai_value, 
            ai_lookup_value, 
            aide_value, 
            aide_lookup_value) 
            VALUES (?,?,?,  ?,?,?,  ?,?,?,  ?,?,?, ?);
            """
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.AssetAttrRepeatingValueId)
                |> addParam (int64Param row.ChangeRequestId)
                |> addParam (int64Param row.AiAssetId)
                |> addParam (int64Param row.AideAssetId)
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

    
    

    
    // ************************************************************************
    // Table: asset : new assets

    let assetNewInsert (row : AssetNewRow) : IndexedCommand option =
        let sql =
            """
            INSERT INTO asset_change 
            (aide_asset_id, 
            aide_asset_reference, 
            aide_asset_name, 
            aide_common_name, 
            aide_installed_from_date, 
            aide_manufacturer, 
            aide_model, 
            aide_hierarchy_key, 
            aide_asset_status, 
            aide_location_reference, 
            aide_asset_deleted) 
            VALUES 
            (?,?,?,  ?,?,?,  ?,?,?,  ?,?);
            """
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.AideAssetId)
                |> addParam (stringParam row.AideAssetReference)
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

    let insertAssetNewRow (row : AssetNewRow) : SqliteDb<unit> = 
        match assetNewInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()

    let insertAssetNewRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAssetNewExport csvPath)
            return! withTransaction <| sforMz table.Rows insertAssetNewRow
        }


    // ************************************************************************
    // Table: change_request_attribute : new attributes

    let attributeNewInsert (row : AttributeNewRow) : IndexedCommand option =
        let sql =
            """
            INSERT INTO asset_attribute_change
            (aide_asset_attr_value_id, 
            aide_asset_id,
            asset_reference, 
            asset_name, 
            asset_common_name, 
            attribute_name, 
            aide_value, 
            aide_lookup_value) 
            VALUES (?,?,?,  ?,?,?,  ?,?);
            """
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.AssetAttrValueId)
                |> addParam (int64Param row.AideAssetId)
                |> addParam (stringParam row.AssetReference)
                |> addParam (stringParam row.AssetName)
                |> addParam (stringParam row.AssetCommonName)
                |> addParam (stringParam row.AttributeName)
                |> addParam (optionNull stringParamAux row.AideValue)
                |> addParam (optionNull stringParamAux row.AideLookupValue)
        cmd |> Some

    let insertAttributeNewRow (row : AttributeNewRow) : SqliteDb<unit> = 
        match attributeNewInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()


    let insertAttributeNewRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAttributeNewExport csvPath)
            return! withTransaction <| sforMz table.Rows insertAttributeNewRow
        }

    // ************************************************************************
    // Table: ai_structure_relationships


    let makeAiStructRelInsert (row : AiStructureRelationshipRow) : IndexedCommand option =
        let sql = 
            """
            INSERT INTO ai_structure_relationships 
            (ai_structure_relationship_id, 
            parent_id, 
            child_id)
            VALUES (?,?,?);
            """
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.StructureRelationshipId)
                |> addParam (int64Param row.ParentAssetId)
                |> addParam (int64Param row.ChildAssetId)
        cmd |> Some


    let insertAiStructRelationshipRow (row : AiStructureRelationshipRow) : SqliteDb<unit> = 
        match makeAiStructRelInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()

    let insertAiStructRelationshipRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! rows = 
                liftOperationResult (fun _ -> getAiStructureRelationshipRows csvPath)
            return! withTransaction <| sforMz rows insertAiStructRelationshipRow
        }


    // ************************************************************************
    // Table: aide_structure_relationships

    let makeAideStructRelInsert (row : AideStructureRelationshipRow) : IndexedCommand option =
        let sql = 
            """
            INSERT INTO aide_structure_relationships
            (aide_structure_relationship_id, 
            parent_id, 
            child_id)
            VALUES (?, ?,?);
            """
        let cmd = 
            new IndexedCommand(commandText = sql)
                |> addParam (int64Param row.StructureRelationshipId)
                |> addParam (int64Param row.ParentAideAssetId)
                |> addParam (int64Param row.ChildAideAssetId)
        cmd |> Some


    let insertAideStructRelationshipRow (row : AideStructureRelationshipRow) : SqliteDb<unit> = 
        match makeAideStructRelInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()

    let insertAideStructRelationshipRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! rows = 
                liftOperationResult (fun _ -> getAideStructureRelationshipRows csvPath)
            return! withTransaction <| sforMz rows insertAideStructRelationshipRow
        }

    