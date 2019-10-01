// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module BuildReport2 =
    
    open System.Data.SQLite

    open SLSqlite.Core

    open AideSync.Base.Addendum
    open AideSync.Attributes
    open AideSync.Datatypes2
    open AideSync.StructureDiff 
    open AideSync.BasicQueries2      
    

    // ************************************************************************
    // Change Scheme Info

    let getChangeSchemeInfo (schemeCode: string) : SqliteDb<ChangeSchemeInfo> =
        let sql = 
            """
            SELECT 
                        scheme.scheme_id            AS [scheme_id],
                        scheme.scheme_code          AS [scheme_code],
                        scheme.scheme_name          AS [scheme_name],
                        scheme.solution_provider    AS [solution_provider]
            FROM work_scheme    AS scheme
            WHERE scheme.scheme_code = :schemecode
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "schemecode" (stringParam schemeCode) 
                   
        let readRow (reader : ResultItem) : ChangeSchemeInfo = 
            { SchemeId = reader.GetInt64(0)
            ; SchemeCode = reader.GetString(1)
            ; Name = reader.GetString(2)
            ; SolutionProvider = reader.GetString(3)
            }

        queryKeyed cmd (Strategy.Head readRow) 

    // ************************************************************************
    // Change Request ids

    let getSchemeChangeRequestIds (schemeCode: string) : SqliteDb<int64 list> =
        let sql = 
            """
            SELECT 
                    changes.change_request_id       AS [change_request_id]
            FROM    view_scheme_change_requests     AS changes
            JOIN    work_scheme    AS scheme    ON changes.scheme_id = scheme.scheme_id
            WHERE 
                    scheme.scheme_code = :schemecode
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "schemecode" (stringParam schemeCode) 
                   
        let readRow1 (reader : ResultItem) : int64 = 
            reader.GetInt64(0)
            
        queryKeyed cmd (Strategy.ReadAll readRow1)


    // ************************************************************************
    // Change Request Info

    let getChangeRequestInfo (chreqId : int64) : SqliteDb<ChangeRequestInfo> =
        let sql = 
            """
            SELECT 
                    chreq.change_request_id        AS [change_request_id],
                    chreq.change_request_type      AS [request_type],
                    chreq.change_request_status    AS [request_status],
                    chreq.comments                 AS [comment],
                    chreq.change_request_time      AS [request_time]
            FROM    change_request AS chreq
            WHERE
                    chreq.change_request_id = :chreqid
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "chreqid" (int64Param chreqId) 
                   
        let readRow (reader : ResultItem) : ChangeRequestInfo = 
            { ChangeRequestId = reader.GetInt64(0)
            ; RequestType = reader.GetString(1)
            ; Status = reader.GetString(2)
            ; Comment = reader.GetString(3)
            ; RequestTime = reader.GetDateTime(4)
            }

        queryKeyed cmd (Strategy.Head readRow)


    // ************************************************************************
    // Find ai asset references for a change request

    /// Assumption - at most one index for a key
    /// The references are the AI uid, not the AIDE uid
    let getChangeRequestAIRootIds (changeRequestId : int64) : SqliteDb<int64 list> = 
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
        
        let readRow (result : ResultItem) : int64 = result.GetInt64(0)

        queryKeyed cmd (Strategy.ReadAll readRow) 


    // ************************************************************************
    // Build Structure diffs


    let buildHierarchyDiffs (changeRequestId : int64) 
                            (aiAssetId : int64) : SqliteDb<Hierarchy<FlocDiff> option>  = 
            sqliteDb {
                let! aiTree = findAiDescendants aiAssetId
                let! aideId = findAideAssetId changeRequestId aiAssetId |> getOptional
                let! aideTree = findAideDescendants aideId
                return diffLists aiTree aideTree |> buildTree
            }
    
    // ************************************************************************
    // Get Properties / Attributes

    
    let getAssetPropertyChanges (aideAssetId : int64) : SqliteDb<PropertyDiffs> = 
        let sql = 
            """
            SELECT 
                change.ai_asset_name            AS [ShortNameL],
                change.aide_asset_name          AS [ShortNameR],
                change.ai_common_name           AS [CommonNameL],
                change.aide_common_name         AS [CommonNameR],
                change.ai_installed_from_date   AS [InstalledFromDateL],
                change.aide_installed_from_date AS [InstalledFromDateR],
                change.ai_manufacturer          AS [ManufacturerL],
                change.aide_manufacturer        AS [ManufacturerR],
                change.ai_model                 AS [ModelL],
                change.aide_model               AS [ModelR], 
                change.ai_hierarchy_key         AS [HierarchyKeyL],
                change.aide_hierarchy_key       AS [HierarchyKeyR], 
                change.ai_asset_status          AS [AssetStatusL],
                change.aide_asset_status        AS [AssetStatusR], 
                change.ai_location_reference    AS [LocRefL],
                change.aide_location_reference  AS [LocRefR], 
                change.ai_asset_deleted         AS [AssetDeletedL],
                change.aide_asset_deleted       AS [AssetDeletedR]
            FROM    asset_change   AS change
            WHERE   change.aide_asset_id = :aideid;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "aideid" (int64Param aideAssetId)
        
        let readRow (result : ResultItem) : PropertyDiffs = 
            let getField (colName : string) : string option = result.TryGetString(colName)
            let getFieldi (colName : string) : string option = 
                result.TryGetInt16(colName) |> Option.map (fun x -> x.ToString())
            [] 
                |> addDifference "Name" (getField "ShortNameL") (getField "ShortNameR")
                |> addDifference "Common Name" (getField "CommonNameL") (getField "CommonNameR")
                |> addDifference "Installed From Date" (getField "InstalledFromDateL") (getField "InstalledFromDateR")
                |> addDifference "Manufacturer" (getField "ManufacturerL") (getField "ManufacturerR")
                |> addDifference "Model" (getField "ModelL") (getField "ManufacturerR")
                |> addDifference "Hierarchy Key" (getField "HierarchyKeyL") (getField "HierarchyKeyR")
                |> addDifference "Asset Status" (getField "AssetStatusL") (getField "AssetStatusR")
                |> addDifference "Loc Ref" (getField "LocRefL") (getField "LocRefR")
                // AssetDeleted is a TINYINT
                |> addDifference "Asset Deleted" (getFieldi "AssetDeletedL") (getFieldi "AssetDeletedR")
                |> List.rev

        queryKeyed cmd (Strategy.Head readRow) <|> mreturn []


    // Asset Atrribute Changes

    // send in cmd for attr value or repeated attr value
    let internal getAttrChanges (cmd : KeyedCommand) : SqliteDb<AttributeDiffs> = 
        let best (value1 : string option) (value2 : string option) : string option = 
            match value2 with
            | Some "NULL" -> value1
            | Some _ -> value2
            | None -> value1

        let readRow (acc : AttributeDiffs) (result : ResultItem) : AttributeDiffs = 
            match result.TryGetString("AttributeName") with
            | None -> acc
            | Some name ->
                let left = best (result.TryGetString("ValueL1")) (result.TryGetString("ValueL2"))
                let right = best (result.TryGetString("ValueR1")) (result.TryGetString("ValueR2"))
                acc
                    |> addDifference name left right
                
        let strategy : Strategy<AttributeDiffs> = Strategy.Fold readRow []
        queryKeyed cmd strategy <|> mreturn []


    let getAssetAttributeChanges (aideAssetId : int64) : SqliteDb<AttributeDiffs> = 
        let sql = 
            """
            SELECT 
                attr_change.attribute_name      AS [AttributeName],
                attr_change.ai_value            AS [ValueL1],
                attr_change.ai_lookup_value     AS [ValueL2],
                attr_change.aide_value          AS [ValueR1],
                attr_change.aide_lookup_value   AS [ValueR2]
            FROM    asset_attribute_change   AS attr_change
            WHERE   
                    attr_change.aide_asset_id = :aideid;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "aideid" (int64Param aideAssetId)
        getAttrChanges cmd

    let getAssetRepeatedAttributeChanges (aideAssetId : int64) : SqliteDb<AttributeDiffs> = 
        let sql = 
            """
            SELECT 
                rep_attr_change.attribute_name      AS [AttributeName],
                rep_attr_change.ai_value            AS [ValueL1],
                rep_attr_change.ai_lookup_value     AS [ValueL2],
                rep_attr_change.aide_value          AS [ValueR1],
                rep_attr_change.aide_lookup_value   AS [ValueR2]
            FROM    asset_repeated_attribute_change   AS rep_attr_change
            WHERE   
                    rep_attr_change.aide_asset_id = :aideid;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "aideid" (int64Param aideAssetId)
        getAttrChanges cmd


    let getNodeChanges (aideAssetId : int64) : SqliteDb<NodeChanges> = 
        sqliteDb {
            let! props = getAssetPropertyChanges aideAssetId
            let! attrs = getAssetAttributeChanges aideAssetId
            let! repAttrs = getAssetRepeatedAttributeChanges aideAssetId
            return { PropertyChanges = props 
                     AttributeChanges = attrs
                     RepeatedAttributeChanges = repAttrs
                    }
        }
    // ************************************************************************
    // Expand FlocDiff nodes filling out their Properties and Attributes

    let expandFlocDiff (flocNode : FlocDiff) : SqliteDb<StructureNode> = 
        sqliteDb { 
            match flocNode with
            | InLeft body -> return Deleted(body)
            | InBoth (b1,b2) -> 
                let! changes = getNodeChanges b2.AideAssetId
                return Common(b1,b2, changes)
            | InRight body -> 
                let! changes = getNodeChanges body.AideAssetId
                return Added(body, changes)
        }

    
    /// In CPS...
    let expandFlocDiffHierarchy (hierarchy : Hierarchy<FlocDiff>) : SqliteDb<Hierarchy<StructureNode>> = 
        let rec work tree mcont : SqliteDb<Hierarchy<StructureNode>>=
            match tree with
            | HierarchyNode(label,kids) -> 
                expandFlocDiff label >>= fun node -> 
                workList kids (fun kids1 -> 
                mcont (HierarchyNode(node,kids1)))
            
        and workList kids mcont : SqliteDb<Hierarchy<StructureNode>>= 
            match kids with 
                | [] -> mcont []
                | k1 :: rest -> 
                    work k1 (fun v1 -> 
                    workList rest (fun vs ->
                    mcont (v1 :: vs)))
        work hierarchy (fun x -> mreturn x)