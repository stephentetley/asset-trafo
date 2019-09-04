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
    let extractAssetPropertyChange (answerRow : ResultItem) 
                                    (propertyDescription : string)
                                    (leftFieldName : string) 
                                    (rightFieldName : string) : AssetPropertyDelta option = 
        let name1 = (valueByName answerRow leftFieldName).ToString()
        let name2 = (valueByName answerRow rightFieldName).ToString()
        if name1 <> name2 then
            { PropertyName = propertyDescription
            ; AiValue = name1
            ; AideValue = name2
            } |> Some
        else None


    let extractAssetPropertyChanges (answerRow : ResultItem) : AssetPropertyDelta list = 
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
            |> List.map (fun (name, ai, aide) -> 
                            extractAssetPropertyChange answerRow name ai aide)
            |> List.choose id 


    let getAssetChanges (chreqId : int64) 
                        (assetId : int64) : SqliteDb<AssetPropertyDelta list> =         
        let sql : string = 
            """ 
            SELECT
                        asset_change.*
            FROM        asset_change       AS asset_change
            WHERE
                        asset_change.change_request_id = :chreqId
            AND         asset_change.ai_asset_id = :assetid
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "chreqid" (int64Param chreqId)   
                |> addNamedParam "assetid" (int64Param assetId) 

        /// One row has a (small) number of property values
        let readRow1 (reader : ResultItem) : AssetPropertyDelta list = 
            extractAssetPropertyChanges reader
            
        /// TODO - joining all rows with concat here is suspect...
        queryKeyed cmd (Strategy.ReadAll readRow1) |>> List.concat
        

    // ************************************************************************
    // Attribute changes



    let getAttributeChanges (chreqId : int64) 
                            (assetId : int64) : SqliteDb<AttributeDelta list> =
        let sql = 
            """
            SELECT 
                    attr.attribute_name       AS [attibute_name],
                    attr.ai_value             AS [ai_value],
                    attr.ai_lookup_value      AS [ai_lookup_value],
                    attr.aide_value           AS [aide_value],
                    attr.aide_lookup_value    AS [aide_lookup_value]
            FROM    asset_attribute_change AS attr
            WHERE
                    attr.change_request_id = :chreqid
            AND     attr.asset_id = :assetid
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "chreqid" (int64Param chreqId) 
                |> addNamedParam "assetid" (int64Param assetId) 
                   
        let readRow1 (reader : ResultItem) : AttributeDelta = 
            let aiValue = 
                match reader.TryGetString(2) with
                | None -> reader.TryGetString(1) |> Option.defaultValue "" |> Lookup
                | Some value -> printfn "%s" value; value |> Literal
            let aideValue = 
                match reader.TryGetString(4) with
                | None -> reader.TryGetString(3) |> Option.defaultValue "" |> Lookup
                | Some value -> value |> Literal
            { AttributeName = reader.GetString(0)
            ; AiValue = aiValue
            ; AideValue = aideValue }
            
                   
        queryKeyed cmd (Strategy.ReadAll readRow1)



    // ************************************************************************
    // Repeated Attribute changes


    let getRepeatedAttributeChanges (chreqId : int64) 
                                    (assetId : int64) : SqliteDb<RepeatedAttributeDelta list> =
        let sql = 
            """
            SELECT 
                    rep_attr.attribute_name       AS [attibute_name],
                    rep_attr.attribute_set_name   AS [attibute_set_name],
                    rep_attr.ai_value             AS [ai_value],
                    rep_attr.ai_lookup_value      AS [ai_lookup_value],
                    rep_attr.aide_value           AS [aide_value],
                    rep_attr.aide_lookup_value    AS [aide_lookup_value]
            FROM    asset_repeated_attribute_change AS rep_attr
            WHERE
                    rep_attr.change_request_id = :chreqid
            AND     rep_attr.asset_id = :assetid
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "chreqid" (int64Param chreqId) 
                |> addNamedParam "assetid" (int64Param assetId) 
            
        let readRow1 (reader : ResultItem) : RepeatedAttributeDelta = 
            let aiValue = 
                match reader.TryGetString(3) with
                | None -> reader.TryGetString(2) |> Option.defaultValue "" |> Lookup
                | Some value -> printfn "%s" value; value |> Literal
            let aideValue = 
                match reader.TryGetString(5) with
                | None -> reader.TryGetString(4) |> Option.defaultValue "" |> Lookup
                | Some value -> value |> Literal
            { RepeatedAttributeName = reader.GetString(0)
            ; AttributeSetName = reader.GetString(1)
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

    // ************************************************************************
    // AssetInfo

    let getAssetInfo (assetId : int64) : SqliteDb<AssetInfo option> =
        let sql = 
            """
            SELECT 
                    ai_asset.reference                AS [Reference],
                    ai_asset.asset_name               AS [Name],
                    ai_asset.asset_common_name        AS [CommonName]
            FROM    
                    ai_asset        AS ai_asset
            WHERE
                    ai_asset.ai_asset_id = :assetid
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "assetid" (int64Param assetId) 
                   
        let readRow1 (reader : ResultItem) : AssetInfo = 
            { AssetId = assetId 
              AssetReference = reader.GetString(0)
              AssetName = reader.GetString(1)
              CommonName = reader.GetString(2)
            }

        queryKeyed cmd (Strategy.ReadAll readRow1) |>> List.tryHead

    // ************************************************************************
    // Differences

    let getDifferences (chreqId : int64) 
                        (assetId : int64) : SqliteDb<Differences>= 
        sqliteDb { 
            let! ai = findAiHierarchy assetId
            let! aide = findAideHierarchy chreqId assetId
            return diffLists ai.Items aide.Items
        }

    let getAssetChangeset (chreqId : int64) 
                            (assetId : int64) : SqliteDb<AssetChangeset> = 
        
        sqliteDb {
            let! assetDeltas = getAssetChanges chreqId assetId
            let! attrDeltas = getAttributeChanges chreqId assetId
            let! repAttrDeltas = getRepeatedAttributeChanges chreqId assetId
            return { AssetProperties = assetDeltas
                     AttrChanges = attrDeltas
                     RepeatedAttrChanges = repAttrDeltas }
        }


    let getAssetChange (chreqId : int64) 
                        (assetId : int64) : SqliteDb<AssetChange option> = 
        sqliteDb {
            match! getAssetInfo assetId with
            | Some info -> 
                let! changes = getAssetChangeset chreqId assetId 
                return (Some { AssetInfo = info; AssetChanges = changes })
            | None -> return None
        }


    let assetStructureChange (chreqId : int64) 
                             (assetId : int64) : SqliteDb<AssetStructureChange option>= 
        sqliteDb { 
            match! getAssetInfo assetId with
            | None -> return None
            | Some info -> 
                let! diffs = getDifferences chreqId assetId            
                return (Some { AssetInfo = info 
                             ; StructureChanges = diffs
                             ; KidsChanges = [] })
        }

    let buildChangeRequest (chreqId : int64) : SqliteDb<ChangeRequest option> =
        sqliteDb { 
            let! assetIds = findChangeRequestAssetIds chreqId
            match! getChangeRequestInfo chreqId with
            | None -> return None
            | Some info when info.RequestType = "Attribute" ->
                let! changes = mapM (getAssetChange chreqId) assetIds |>> List.choose id
                return Some (AttributeChange(info, changes))

            | Some info when info.RequestType = "AIDE" ->
                let! changes = mapM (assetStructureChange chreqId) assetIds |>> List.choose id
                return Some (AideChange(info, changes))
            | Some info ->
                return Some (UnhandledChangeRequest(info))
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