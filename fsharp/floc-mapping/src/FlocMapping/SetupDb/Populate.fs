// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.SetupDb

module Populate = 
    
    open FSharp.Data

    open SLSqlite.Core
    
    open FlocMapping.SetupDb.ImportSchema

    // ************************************************************************
    // Table: aib_floc

    let aibFlocInsert (row : AibFlocRow) : IndexedCommand =
        let sql =        
            """
            INSERT INTO aib_floc
            (sai_ref, 
            short_name, 
            short_code, 
            category, 
            asset_type, 
            parent_ref) 
            VALUES (?,?,?,  ?,?,?);
            """
        new IndexedCommand(commandText = sql)
            |> addParam (stringParam row.Reference)
            |> addParam (stringParam row.AssetName)
            |> addParam (stringParam row.AssetCode)
            |> addParam (stringParam row.Category)
            |> addParam (stringParam row.AssetType)
            |> addParam (stringParam row.ParentRef)
        


    let insertAibFlocRows (csvPath : string) : SqliteDb<unit> = 
        let insertRow row = executeNonQueryIndexed (aibFlocInsert row) |>> ignore
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAibFlocExport csvPath)
            return! withTransaction <| smapMz insertRow table.Rows 
        }

        
    // ************************************************************************
    // Table: aib_equipment

    let aibEquipmentInsert (row : AibEquipmentRow) : IndexedCommand =
        let sql =        
            """
            INSERT INTO aib_equipment
            (pli_ref, 
            short_name, 
            category, 
            equipment_type, 
            parent_ref) 
            VALUES (?,?,?,  ?,?);
            """
        new IndexedCommand(commandText = sql)
            |> addParam (stringParam row.Reference)
            |> addParam (stringParam row.AssetName)
            |> addParam (stringParam row.Category)
            |> addParam (stringParam row.AssetType)
            |> addParam (stringParam row.ParentRef)


    
        

    let insertAibEquipmentRows (csvPath : string) : SqliteDb<unit> = 
        let insertRow row = 
            executeNonQueryIndexed (aibEquipmentInsert row) |>> ignore

        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> readAibEquipmentExport csvPath)
            return! withTransaction <| smapMz insertRow table.Rows 
        }