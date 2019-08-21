// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module BuildReport =
    
    open SLSqlite.Core

    open AssetSync.ChangesReport.Addendum
    open AssetSync.ChangesReport.Datatypes


    /// D'oh - strings
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


    let getAssetPropertyChanges (answerRow : RowReader) : AssetProperty list = 
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

