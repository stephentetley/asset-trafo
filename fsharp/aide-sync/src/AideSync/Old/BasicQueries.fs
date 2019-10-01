// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.Old


module BasicQueries =

    
    open SLSqlite.Core

    open AideSync.Base.Addendum
    open AideSync.Old.Datatypes

    // ************************************************************************
    // Find (numeric) id from SAI reference

    /// Assumption - at most one index for a key
    let findAiAssetId (reference : string) : SqliteDb<int64 option> = 
        let sql = 
            """
            SELECT 
                    asset.ai_asset_id        AS [AssetId]
            FROM    ai_asset       AS asset
            WHERE
                    asset.reference = :sairef
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "sairef" (stringParam reference)
        
        let readRow1 (result : ResultItem) : int64 option = result.TryGetInt64(0)

        queryKeyed cmd (Strategy.Head readRow1) 

    // ************************************************************************
    // Find CommonName from SAI reference

    /// Assumption - at most CommonName for a key
    let findAiCommonName (assetId : int64)  : SqliteDb<string option> = 
        let sql = 
            """
            SELECT 
                    asset.asset_common_name        AS [CommonName]
            FROM    ai_asset       AS asset
            WHERE
                    asset.asset_id = :assetid
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "assetid" (int64Param assetId)
        
        let readRow1 (result : ResultItem) : string option = result.TryGetString(0)

        queryKeyed cmd (Strategy.Head readRow1) 


    // ************************************************************************
    // Find asset references for a change request

    /// Assumption - at most one index for a key
    /// The references are the AI uid, not the AIDE uid
    let findChangeRequestAssetIds (changeRequestId : int64) : SqliteDb<int64 list> = 
        let sql = 
            """
            SELECT 
                    change.ai_asset_id      AS [AssetId]
            FROM    asset_change            AS change
            WHERE
                    change.change_request_id = :chreq
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "chreq" (int64Param changeRequestId)
        
        let readRow1 (result : ResultItem) : int64 = result.GetInt64(0)

        queryKeyed cmd (Strategy.ReadAll readRow1) 


    // ************************************************************************
    // Find kids (AI)
            
    let findAiDescendants (startId : int64) : SqliteDb<AiStructureItem list> =
        let sql = 
            """
            WITH RECURSIVE
            temp_table(child_id) AS (
                SELECT :startid
                    UNION ALL
                SELECT ai_structure_relationships.child_id
                FROM ai_structure_relationships, temp_table
                WHERE ai_structure_relationships.parent_id = temp_table.child_id
                )
            SELECT 
                temp_table.child_id AS [AssetId],
                asset.asset_name AS [Name],
                asset.asset_common_name AS [CommonName],
                asset.reference AS [Reference]
            FROM temp_table
            JOIN ai_asset    AS asset ON temp_table.child_id = asset.ai_asset_id
            ORDER BY asset.asset_common_name
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "startid" (int64Param startId)
        
        let readRow1 (result : ResultItem) : AiStructureItem = 
            { AssetId = result.GetInt64(0)
            ; Name = result.GetString(1)
            ; CommonName = result.GetString(2)            
            ; Reference = result.GetString(3)
            }

        queryKeyed cmd (Strategy.ReadAll readRow1) 
    

    // ************************************************************************
    // Find (numeric) id from SAI reference

    /// Assumption - at most one index for the pair (change_request_id * key)
    let findAideAssetId (changeRequestId : int64) 
                           (assetId : int64) : SqliteDb<int64 option> = 
        let sql = 
            """
            SELECT 
                    aide.aide_asset_id     AS [AideAssetId]
            FROM    aide_asset             AS aide
            WHERE
                    aide.change_request_id = :chreq 
            AND     aide.asset_id = :assetid     
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "chreq" (int64Param changeRequestId)
                |> addNamedParam "assetid" (int64Param assetId)
        
        let readRow1 (result : ResultItem) : int64 = result.GetInt64(0)

        queryKeyed cmd (Strategy.Head readRow1) |> optional

    // ************************************************************************
    // Find kids (AIDE)
            
    let findAideDescendants (startId : int64) : SqliteDb<AideStructureItem list> =
        let sql = 
            """
            WITH RECURSIVE
            temp_table(child_id) AS (
                SELECT :startid
                    UNION ALL
                SELECT aide_structure_relationships.child_id
                FROM aide_structure_relationships, temp_table
                WHERE aide_structure_relationships.parent_id = temp_table.child_id
                )
            SELECT 
                temp_table.child_id AS [AideAssetId],
                asset.asset_name AS [Name],
                asset.asset_common_name AS [CommonName],
                asset.reference AS [Reference]
            FROM temp_table
            JOIN aide_asset    AS asset ON temp_table.child_id = asset.aide_asset_id
            ORDER BY asset.asset_common_name
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "startid" (int64Param startId)
        
        let readRow1 (result : ResultItem) : AideStructureItem =
            { AideAssetId = result.GetInt64(0)
            ; Name = result.GetString(1)
            ; CommonName = result.GetString(2)
            ; Reference = result.GetString(3)
            }

        queryKeyed cmd (Strategy.ReadAll readRow1) 



    let findAiHierarchy (assetId : int64) : SqliteDb<AiHierarchy> =
        findAiDescendants assetId |>> AiHierarchy



    let findAideHierarchy (changeRequestId : int64) 
                          (assetId : int64) : SqliteDb<AideHierarchy> =
        sqliteDb { 
            match! findAideAssetId changeRequestId assetId with
            | None -> return (AideHierarchy [])
            | Some key -> return! findAideDescendants key |>> AideHierarchy
        }


