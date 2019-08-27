// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module BuildReport =
    
    open System.Data.SQLite

    open SLSqlite.Core

    open AssetSync.Base.Addendum
    open AssetSync.ChangesReport.Datatypes


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
                cr_asset.*
            FROM        change_request_asset AS cr_asset
            WHERE
                    cr_asset.change_request_id = :chreqid
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
                    cr_attrib.asset_name           AS [asset_name],
                    cr_attrib.asset_reference      AS [reference],
                    cr_attrib.attribute_name       AS [attibute_name],
                    cr_attrib.ai_value             AS [ai_value],
                    cr_attrib.ai_lookup_value      AS [ai_lookup_value],
                    cr_attrib.aide_value           AS [aide_value],
                    cr_attrib.aide_lookup_value    AS [aide_lookup_value]
            FROM    change_request_attribute AS cr_attrib
            WHERE
                    cr_attrib.change_request_id = :chreqid
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
                    cr_rep_attr.asset_name           AS [asset_name],
                    cr_rep_attr.asset_reference      AS [reference],
                    cr_rep_attr.attribute_name       AS [attibute_name],
                    cr_rep_attr.attribute_set_name   AS [attibute_set_name],
                    cr_rep_attr.ai_value             AS [ai_value],
                    cr_rep_attr.ai_lookup_value      AS [ai_lookup_value],
                    cr_rep_attr.aide_value           AS [aide_value],
                    cr_rep_attr.aide_lookup_value    AS [aide_lookup_value]
            FROM    change_request_repeated_attribute AS cr_rep_attr
            WHERE
                    cr_rep_attr.change_request_id = :chreqid
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

    
    let buildChangeRequest  (chreqId : int64) : SqliteDb<ChangeRequest option> =
        sqliteDb { 
            match! getChangeRequestInfo chreqId with
            | None -> return None
            | Some info ->
                let! xs = getAssetChanges chreqId
                let! ys = getAttributeChanges chreqId 
                let! zs = getRepeatedAttributeChanges chreqId
                let ans = 
                    { Info = info
                    ; AssetChanges = xs
                    ; AttributeChanges = ys
                    ; RepeatedAttributeChanges = zs
                    }
                return (Some ans)
        }

