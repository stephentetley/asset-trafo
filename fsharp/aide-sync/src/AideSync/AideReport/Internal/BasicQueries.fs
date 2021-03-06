﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.AideReport.Internal


module BasicQueries =

    
    open SLSqlite.Core

    open AideSync.Base.Addendum
    open AideSync.AideReport.Internal.Datatypes




    // ************************************************************************
    // Find kids (AI)
            
    let findAiDescendants (aiStartId : int64) : SqliteDb<AiFlocNode list> =
        let sql = 
            """
            WITH RECURSIVE
            temp_table(child_id, parent_id, node_level) AS (
                SELECT :startid, 0, 1
                    UNION ALL
                SELECT 
                    ai_structure_relationships.child_id,
                    ai_structure_relationships.parent_id,
                    node_level + 1
                FROM ai_structure_relationships, temp_table
                WHERE ai_structure_relationships.parent_id = temp_table.child_id
                )
            SELECT 
                temp_table.child_id AS [AssetId],
                asset.reference AS [Reference],
                temp_table.node_level AS [NodeLevel],
                parent_asset.reference AS [ParentReference],
                asset.asset_name AS [Name],
                asset.asset_common_name AS [CommonName]
            FROM temp_table
            JOIN ai_asset    AS asset ON temp_table.child_id = asset.ai_asset_id
            LEFT OUTER JOIN ai_asset    AS parent_asset ON temp_table.parent_id = parent_asset.ai_asset_id            
            ORDER BY asset.asset_common_name
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "startid" (int64Param aiStartId)
        
        let readRow1 (result : ResultItem) : AiFlocNode = 
            { AssetId = result.GetInt64(0)
              Reference = result.GetString(1)
              HierarchyLevel = result.GetInt32(2)
              ParentReference = result.TryGetString(3) |> Option.defaultValue ""
              ShortName = result.GetString(4)
              CommonName = result.GetString(5)
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
                    changes.aide_asset_id     AS [AideAssetId]
            FROM    asset_change           AS changes
            WHERE
                    changes.change_request_id = :chreq 
            AND     changes.ai_asset_id = :assetid     
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "chreq" (int64Param changeRequestId)
                |> addNamedParam "assetid" (int64Param assetId)
        
        let readRow1 (result : ResultItem) : int64 = result.GetInt64(0)

        queryKeyed cmd (Strategy.Head readRow1) |> optional 
            <?> sprintf "findAideAssetId failed"


    // ************************************************************************
    // Find kids (AIDE)
            
    let findAideDescendants (aideStartId : int64) : SqliteDb<AideFlocNode list> =
        let sql = 
            """
            WITH RECURSIVE
            temp_table(child_id, parent_id, node_level) AS (
                SELECT :startid, 0, 1
                    UNION ALL
                SELECT 
                    aide_structure_relationships.child_id,
                    aide_structure_relationships.parent_id,
                    node_level + 1
                FROM aide_structure_relationships, temp_table
                WHERE aide_structure_relationships.parent_id = temp_table.child_id
                )
            SELECT 
                temp_table.child_id AS [AideAssetId],
                asset.reference AS [Reference],
                temp_table.node_level AS [NodeLevel],
                parent_asset.reference AS [ParentReference],
                asset.asset_name AS [Name],
                asset.asset_common_name AS [CommonName]
            FROM temp_table
            JOIN aide_asset    AS asset ON temp_table.child_id = asset.aide_asset_id
            LEFT OUTER JOIN aide_asset    AS parent_asset ON temp_table.parent_id = parent_asset.aide_asset_id
            ORDER BY asset.asset_common_name
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "startid" (int64Param aideStartId)
        
        let readRow1 (result : ResultItem) : AideFlocNode =
            { AideAssetId = result.GetInt64(0)
            ; Reference = result.GetString(1)
            ; HierarchyLevel = result.GetInt32(2)
            ; ParentReference = result.TryGetString(3) |> Option.defaultValue ""
            ; ShortName = result.GetString(4)
            ; CommonName = result.GetString(5)
            }

        queryKeyed cmd (Strategy.ReadAll readRow1) 
            <?> "findAideDescendants failed"



