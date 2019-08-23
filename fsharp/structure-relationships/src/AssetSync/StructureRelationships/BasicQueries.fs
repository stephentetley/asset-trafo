// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.StructureRelationships


module BasicQueries =

    open FSharp.Data
    
    open SLSqlite.Core

    open AssetSync.Base.Addendum        

    // ************************************************************************
    // Find (numeric) id from SAI reference

    /// Assumption - at most one index for a key
    let findAiAssetIndex (reference : string) : SqliteDb<int64 option> = 
        let sql = 
            """
            SELECT 
                    ai_assets.asset_id        AS [AssetId]
            FROM    ai_asset_lookups          AS ai_assets
            WHERE
                    ai_assets.reference = :sairef
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "sairef" (stringParam reference)
        
        let readRow1 (result : ResultItem) : int64 = result.GetInt64(0)

        queryKeyed cmd (Strategy.Head readRow1) |> optional

    
    // ************************************************************************
    // Find kids (AI)
            
    let findAiDescendants (startId : int64) : SqliteDb<(string * string) list> =
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
                    ai_asset_lookups.reference AS [Reference],
                    ai_asset_lookups.asset_common_name AS [CommonName]
                FROM temp_table
                JOIN ai_asset_lookups    ON temp_table.child_id = ai_asset_lookups.asset_id
                ORDER BY ai_asset_lookups.asset_common_name
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "startid" (int64Param startId)
        
        let readRow1 (result : ResultItem) : string * string= 
            result.GetString(1), result.GetString(2)

        queryKeyed cmd (Strategy.ReadAll readRow1) 
    

    // ************************************************************************
    // Find (numeric) id from SAI reference

    /// Assumption - at most one index for the pair (change_request_id * key)
    let findAideAssetIndex (changeRequestId : int64) 
                           (reference : string) : SqliteDb<int64 option> = 
        let sql = 
            """
            SELECT 
                    aide_assets.aide_asset_id   AS [AideAssetId]
            FROM    aide_asset_lookups          AS aide_assets
            WHERE
                    aide_assets.reference = :sairef
            AND     aide_assets.change_request_id = :chreq 
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
            
    let findAideDescendants (startId : int64) : SqliteDb<(string * string) list> =
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
                aide_asset_lookups.reference AS [Reference],
                aide_asset_lookups.asset_common_name AS [CommonName]
            FROM temp_table
            JOIN aide_asset_lookups    ON temp_table.child_id = aide_asset_lookups.aide_asset_id
            ORDER BY aide_asset_lookups.asset_common_name
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "startid" (int64Param startId)
        
        let readRow1 (result : ResultItem) : string * string= 
            result.GetString(1), result.GetString(2)

        queryKeyed cmd (Strategy.ReadAll readRow1) 


