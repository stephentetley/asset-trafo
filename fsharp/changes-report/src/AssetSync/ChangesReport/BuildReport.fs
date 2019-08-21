// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module BuildReport =
    
    open System.Data.SQLite

    open SLSqlite.Core

    open AssetSync.ChangesReport.Addendum
    open AssetSync.ChangesReport.Datatypes

    // ************************************************************************
    // Asset 'Property' changes

    /// Note - the field might not have type=string
    /// But it looks like we can naturally show any object
    /// we find with ToString().
    let getAssetPropertyChange (answerRow : RowReader) 
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


    let extractAssetPropertyChanges (answerRow : RowReader) : AssetProperty list = 
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


    let getAssetChanges (chreqid : int64) : SqliteDb<AssetChange list> =         
        let sql : string = 
            """ 
            SELECT
                cr_asset.*
            FROM        change_request_asset AS cr_asset
            WHERE
                    cr_asset.change_request_id = :chreqid
            ;
            """
        
        let readRow1 (answerRow : RowReader) : AssetChange = 
            let propChanges = extractAssetPropertyChanges answerRow
            { Reference = (valueByName answerRow "ai_asset_reference").ToString()
            ; AiAssetName = (valueByName answerRow "ai_asset_name").ToString()
            ; AiCommonName = (valueByName answerRow "ai_common_name").ToString()
            ; AssetProperties = propChanges
            }

        let cmd = new SQLiteCommand(commandText = sql)
        cmd.Parameters.AddWithValue(parameterName = "chreqid", value = box chreqid) |> ignore
            
        
            
    
        executeReader cmd (readerReadAll readRow1)
        
    // ************************************************************************
    // Attribute changes


    let getAttributeChanges (chreqid : int64) : SqliteDb<AttributeChange list> =
        let sql = 
            """
            SELECT 
                    cr_attrib.asset_common_name    AS [common_name],
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
        let cmd = new SQLiteCommand(commandText = sql)
        cmd.Parameters.AddWithValue(parameterName = "chreqid", value = box chreqid) |> ignore
                   
        let readRow1 (reader : RowReader) : AttributeChange = 
            let aiValue = 
                match reader.TryGetString(4) with
                | None -> reader.GetString(3) |> Lookup
                | Some value -> value |> Literal
            let aideValue = 
                match reader.TryGetString(6) with
                | None -> reader.GetString(5) |> Lookup
                | Some value -> value |> Literal
            { AssetName = reader.GetString(0)
            ; Reference = reader.GetString(1)
            ; AttributeName = reader.GetString(2)
            ; AiValue = aiValue
            ; AideValue = aideValue
            }
                   
           
        executeReader cmd (readerReadAll readRow1)