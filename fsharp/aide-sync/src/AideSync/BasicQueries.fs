// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module BasicQueries =

    
    open SLSqlite.Core

    open AideSync.Base.Addendum
    open AideSync.Datatypes

    // ************************************************************************
    // Find (numeric) id from SAI reference

    /// Assumption - at most one index for a key
    let findAiAssetIndex (reference : string) : SqliteDb<int64 option> = 
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
    // Find SAI references for a change request

    /// Assumption - at most one index for a key
    let findChangeAideSairefs (changeRequestId : int64) : SqliteDb<string list> = 
        let sql = 
            """
            SELECT 
                    change.aide_asset_reference    AS [AssetId]
            FROM    asset_change       AS change
            WHERE
                    change.change_request_id = :chreq
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "chreq" (int64Param changeRequestId)
        
        let readRow1 (result : ResultItem) : string = result.GetString(0)

        queryKeyed cmd (Strategy.ReadAll readRow1) 


    // ************************************************************************
    // Find kids (AI)
            
    let findAiDescendants (startId : int64) : SqliteDb<StructureItem list> =
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
                temp_table.child_id AS [ChildId],
                asset.reference AS [Reference],
                asset.asset_name AS [Name],
                asset.asset_common_name AS [CommonName]
            FROM temp_table
            JOIN ai_asset    AS asset ON temp_table.child_id = asset.ai_asset_id
            ORDER BY asset.asset_common_name
            ;
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "startid" (int64Param startId)
        
        let readRow1 (result : ResultItem) : StructureItem = 
            { Reference = result.GetString(1)
            ; Name = result.GetString(2)
            ; CommonName = result.GetString(3)
            }

        queryKeyed cmd (Strategy.ReadAll readRow1) 
    

    // ************************************************************************
    // Find (numeric) id from SAI reference

    /// Assumption - at most one index for the pair (change_request_id * key)
    let findAideAssetIndex (changeRequestId : int64) 
                           (reference : string) : SqliteDb<int64 option> = 
        let sql = 
            """
            SELECT 
                    asset.aide_asset_id     AS [AideAssetId]
            FROM    aide_asset              AS asset
            WHERE
                    asset.reference = :sairef
            AND     asset.change_request_id = :chreq 
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "sairef" (stringParam reference)
                |> addNamedParam "chreq" (int64Param changeRequestId)
        
        let readRow1 (result : ResultItem) : int64 = result.GetInt64(0)

        queryKeyed cmd (Strategy.Head readRow1) |> optional

    // ************************************************************************
    // Find kids (AIDE)
            
    let findAideDescendants (startId : int64) : SqliteDb<StructureItem list> =
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
                temp_table.child_id AS [ChildId],
                asset.reference AS [Reference],
                asset.asset_name AS [Name],
                asset.asset_common_name AS [CommonName]
            FROM temp_table
            JOIN aide_asset    AS asset ON temp_table.child_id = asset.aide_asset_id
            ORDER BY asset.asset_common_name
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "startid" (int64Param startId)
        
        let readRow1 (result : ResultItem) : StructureItem = 
            { Reference = result.GetString(1)
            ; Name = result.GetString(2)
            ; CommonName = result.GetString(3)
            }

        queryKeyed cmd (Strategy.ReadAll readRow1) 



    let findAiHierarchy (reference : string) : SqliteDb<Hierarchy> =
        sqliteDb { 
            match! findAiAssetIndex reference with
            | None -> return (Hierarchy [])
            | Some key -> return! findAiDescendants key |>> Hierarchy
        }


    let findAideHierarchy (changeRequestId : int64) 
                              (reference : string) : SqliteDb<Hierarchy> =
        sqliteDb { 
            match! findAideAssetIndex changeRequestId reference with
            | None -> return (Hierarchy [])
            | Some key -> return! findAideDescendants key |>> Hierarchy
        }


