// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module BuildReport =
    
    open System.Data.SQLite

    open SLSqlite.Core

    open AideSync.Base.Addendum
    open AideSync.Datatypes
    open AideSync.DiffImplementation      
    open AideSync.BasicQueries


    // ************************************************************************
    // Asset 'Property' changes

    /// Note - the field might not have type=string
    /// But it looks like we can naturally show any object
    /// we find with ToString().
    let getAssetPropertyChange (answerRow : ResultItem) 
                                (propertyDescription : string)
                                (leftField : string) 
                                (rightField : string) : AssetProperty option = 
        let name1 = (valueByName answerRow leftField).ToString()
        let name2 = (valueByName answerRow rightField).ToString()
        if name1 <> name2 then
            { PropertyName = propertyDescription
            ; AiValue = name1
            ; AideValue = name2
            } |> Some
        else None


    let extractAssetPropertyChanges (answerRow : ResultItem) : AssetProperty list = 
        let properties = 
            [ ("Asset Name", "ai_asset_name","aide_asset_name")
            ; ("Common Name", "ai_common_name","aide_common_name")
            ; ("Installed FromDate", "ai_installed_from_date","aide_installed_from_date")
            ; ("Manufacturer", "ai_manufacturer", "aide_manufacturer")  
            ; ("Model", "ai_model", "aide_model")  
            ; ("Hierarchy Key", "ai_hierarchy_key", "aide_hierarchy_key")  
            ; ("Asset Status", "ai_asset_status", "aide_asset_status") 
            ; ("Location Reference", "ai_location_reference", "aide_location_reference")
            ; ("Asset Deleted", "ai_asset_deleted", "aide_asset_deleted")
            ]
        properties
            |> List.map (fun (name, ai, aide) -> getAssetPropertyChange answerRow name ai aide)
            |> List.choose id 


    let getAssetChanges (chreqId : int64) : SqliteDb<AssetChange list> =         
        let sql : string = 
            """ 
            SELECT
                        asset.*
            FROM        asset_change AS asset
            WHERE
                        asset.change_request_id = :chreqid
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "chreqid" (int64Param chreqId)   

        let readRow1 (answerRow : ResultItem) : AssetChange = 
            let propChanges = extractAssetPropertyChanges answerRow
            { Reference = (valueByName answerRow "ai_asset_reference").ToString()
            ; AiAssetName = (valueByName answerRow "ai_asset_name").ToString()
            ; AiCommonName = (valueByName answerRow "ai_common_name").ToString()
            ; AssetProperties = propChanges
            }

        queryKeyed cmd (Strategy.ReadAll readRow1)
        
    // ************************************************************************
    // Attribute changes


    let getAttributeChanges (chreqId : int64) : SqliteDb<AttributeChange list> =
        let sql = 
            """
            SELECT 
                    attr.asset_name           AS [asset_name],
                    attr.asset_reference      AS [reference],
                    attr.attribute_name       AS [attibute_name],
                    attr.ai_value             AS [ai_value],
                    attr.ai_lookup_value      AS [ai_lookup_value],
                    attr.aide_value           AS [aide_value],
                    attr.aide_lookup_value    AS [aide_lookup_value]
            FROM    asset_attribute_change AS attr
            WHERE
                    attr.change_request_id = :chreqid
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "chreqid" (int64Param chreqId) 
                   
        let readRow1 (reader : ResultItem) : AttributeChange = 
            let aiValue = 
                match reader.TryGetString(4) with
                | None -> reader.TryGetString(3) |> Option.defaultValue "" |> Lookup
                | Some value -> printfn "%s" value; value |> Literal
            let aideValue = 
                match reader.TryGetString(6) with
                | None -> reader.TryGetString(5) |> Option.defaultValue "" |> Lookup
                | Some value -> value |> Literal
            { AssetName = reader.GetString(0)
            ; Reference = reader.GetString(1)
            ; AttributeName = reader.GetString(2)
            ; AiValue = aiValue
            ; AideValue = aideValue
            }
                   
        queryKeyed cmd (Strategy.ReadAll readRow1)

    // ************************************************************************
    // Repeated Attribute changes


    let getRepeatedAttributeChanges (chreqId : int64) : SqliteDb<RepeatedAttributeChange list> =
        let sql = 
            """
            SELECT 
                    rep_attr.asset_name           AS [asset_name],
                    rep_attr.asset_reference      AS [reference],
                    rep_attr.attribute_name       AS [attibute_name],
                    rep_attr.attribute_set_name   AS [attibute_set_name],
                    rep_attr.ai_value             AS [ai_value],
                    rep_attr.ai_lookup_value      AS [ai_lookup_value],
                    rep_attr.aide_value           AS [aide_value],
                    rep_attr.aide_lookup_value    AS [aide_lookup_value]
            FROM    asset_repeated_attribute_change AS rep_attr
            WHERE
                    rep_attr.change_request_id = :chreqid
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "chreqid" (int64Param chreqId) 
            
        let readRow1 (reader : ResultItem) : RepeatedAttributeChange = 
            let aiValue = 
                match reader.TryGetString(5) with
                | None -> reader.TryGetString(4) |> Option.defaultValue "" |> Lookup
                | Some value -> printfn "%s" value; value |> Literal
            let aideValue = 
                match reader.TryGetString(7) with
                | None -> reader.TryGetString(6) |> Option.defaultValue "" |> Lookup
                | Some value -> value |> Literal
            { AssetName = reader.GetString(0)
            ; Reference = reader.GetString(1)
            ; RepeatedAttributeName = reader.GetString(2)
            ; AttributeSetName = reader.GetString(3)
            ; AiValue = aiValue
            ; AideValue = aideValue
            }
                       
        queryKeyed cmd (Strategy.ReadAll readRow1)

    // ************************************************************************
    // Change Request Info

    let getChangeRequestInfo (chreqId : int64) : SqliteDb<ChangeRequestInfo option> =
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
                   
        let readRow1 (reader : ResultItem) : ChangeRequestInfo = 
            { ChangeRequestId = reader.GetInt64(0)
            ; RequestType = reader.GetString(1)
            ; Status = reader.GetString(2)
            ; Comment = reader.GetString(3)
            ; RequestTime = reader.GetDateTime(4)
            }

        queryKeyed cmd (Strategy.ReadAll readRow1) |>> List.tryHead



    let structureRelationshipsDiff (changeReqId : int64) 
                                   (sairef : string) : SqliteDb<Differences>= 
        sqliteDb { 
            let! ai = findAiHierarchy sairef
            let! aide = findAideHierarchy changeReqId sairef
            return diffLists ai.Items aide.Items
        }

    let assetStructureChange (changeReqId : int64) 
                             (sairef : string) : SqliteDb<AssetStructureChange>= 
        sqliteDb { 
            let! diffs = structureRelationshipsDiff changeReqId sairef
            return { AssetReference = sairef 
                   ; StructureChanges = diffs }
        }

    let buildChangeRequest (chreqId : int64) : SqliteDb<ChangeRequest option> =
        sqliteDb { 
            match! getChangeRequestInfo chreqId with
            | None -> return None
            | Some info ->
                let! assetChanges = getAssetChanges chreqId
                let! attrChanges = getAttributeChanges chreqId 
                let! repAttrChanges = getRepeatedAttributeChanges chreqId
                let! sairefs = findChangeAideSairefs chreqId
                printfn "%O" sairefs
                let! structureChanges = 
                    mapM (assetStructureChange chreqId) sairefs
                let ans = 
                    { Info = info
                    ; AssetChanges = assetChanges
                    ; AttributeChanges = attrChanges
                    ; RepeatedAttributeChanges = repAttrChanges
                    ; StructureRelationshipChanges = structureChanges
                    }
                return (Some ans)
        }

    // ************************************************************************
    // Change Scheme Info

    let getChangeSchemeInfo (schemeCode: string) : SqliteDb<ChangeSchemeInfo option> =
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
                   
        let readRow1 (reader : ResultItem) : ChangeSchemeInfo = 
            { SchemeId = reader.GetInt64(0)
            ; Code = reader.GetString(1)
            ; Name = reader.GetString(2)
            ; SolutionProvider = reader.GetString(3)
            }

        queryKeyed cmd (Strategy.ReadAll readRow1) |>> List.tryHead

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




    let buildChangeScheme (schemeCode : string) : SqliteDb<ChangeScheme option> =
        sqliteDb { 
            match! getChangeSchemeInfo schemeCode with
            | None -> return None
            | Some info ->
                let! crids = getSchemeChangeRequestIds schemeCode
                let! crs = mapM buildChangeRequest crids |>> List.choose id 
                let ans = 
                    { Info = info
                    ; ChangeRequests = crs
                    }
                return (Some ans)
        }