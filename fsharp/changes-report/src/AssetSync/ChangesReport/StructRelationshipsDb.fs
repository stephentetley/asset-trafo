// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module StructRelationshipsDb =

    open System.IO
    open FSharp.Data

    open System.Data.SQLite

    open SLSqlite.Utils
    open SLSqlite.SqliteDb

    open AssetSync.ChangesReport.Addendum



    // ************************************************************************
    // aide_structure_relationships table

    [<Literal>]
    let AideStructureRelationshipSchema = 
        "StructureRelationshipId(int64),\
         ParentAideAssetId(int64),\
         ChildAideAssetId(int64)"    

    [<Literal>]
    let AideStructureRelationshipSample = 
        "1,150,2500"
       

    type AideStructureRelationshipTable = 
        CsvProvider< Schema = AideStructureRelationshipSchema 
                   , Sample = AideStructureRelationshipSample
                   , HasHeaders = true >

    type AideStructureRelationshipRow = AideStructureRelationshipTable.Row

    let getAideStructureRelationshipRows (cvsPath : string) : Result<seq<AideStructureRelationshipRow>, string> = 
        try
            let table = AideStructureRelationshipTable.Load(uri = cvsPath)
            table.Rows |> Ok
        with
        | excn -> Error excn.Message



    let makeStructRelInsert (row : AideStructureRelationshipRow) : SQLiteCommand option =
        let sql = 
            "INSERT INTO aide_structure_relationships (structure_relationship_id, parent_id, child_id) \
            VALUES (:relid, :parent, :child)"
        let cmd = new SQLiteCommand(commandText = sql)
        cmd.Parameters.AddWithValue(parameterName = "relid", value = box row.StructureRelationshipId) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "parent", value = box row.ParentAideAssetId) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "child", value = box row.ChildAideAssetId) |> ignore
        cmd |> Some


    let insertAideStructRelationshipRow (row : AideStructureRelationshipRow) : SqliteDb<unit> = 
        match makeStructRelInsert row with
        | Some statement -> 
            executeNonQuery statement |>> ignore
        | None -> mreturn ()

    let insertAideStructRelationshipRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! rows = 
                liftOperationResult (fun _ -> getAideStructureRelationshipRows csvPath)
            return! withTransaction <| sforMz rows insertAideStructRelationshipRow
        }


    // ************************************************************************
    // aide_asset_lookups table

    [<Literal>]
    let AideAssetLookupSchema = 
        "AideAssetId(int64),\
         AssetId(int64 option),\
         Reference(string),\
         ChangeRequestId(int64 option),\
         AssetName(string),\
         AssetType(string),\
         AssetCategory(string)"    

    [<Literal>]
    let AideAssetLookupSample = 
        "1000,600000,PLI001,NULL,Asset Name, Asset Type, Asset Category"
       

    type AideAssetLookupTable = 
        CsvProvider< Schema = AideAssetLookupSchema 
                   , Sample = AideAssetLookupSample
                   , HasHeaders = true >

    type AideAssetLookupRow = AideAssetLookupTable.Row

    let getAideAssetLookupRows (cvsPath : string) : Result<seq<AideAssetLookupRow>, string> = 
        try
            let table = AideAssetLookupTable.Load(uri = cvsPath)
            table.Rows |> Ok
        with
        | excn -> Error excn.Message



    let makeAideAssetLookupInsert (row : AideAssetLookupRow) : SQLiteCommand option =
        let sql = 
            "INSERT INTO aide_asset_lookups \
            (aide_asset_id, asset_id, reference, change_request_id, asset_name, asset_type, asset_category) \
            VALUES \
            (:aideid, :assetid, :reference, :changeid, :aname, :atype, :acat)"
        let cmd = new SQLiteCommand(commandText = sql)
        cmd.Parameters.AddWithValue(parameterName = "aideid", value = box row.AideAssetId ) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "assetid", value = box row.AssetId) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "reference", value = box row.Reference) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "changeid", value = box row.ChangeRequestId) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "aname", value = box row.AssetName) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "atype", value = box row.AssetType) |> ignore
        cmd.Parameters.AddWithValue(parameterName = "acat", value = box row.AssetCategory) |> ignore
        cmd |> Some


    let insertAideAssetLookupRow (row : AideAssetLookupRow) : SqliteDb<unit> = 
        match makeAideAssetLookupInsert row with
        | Some statement -> 
            executeNonQuery statement |>> ignore
        | None -> mreturn ()

    let insertAideAssetLookupRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! rows = 
                liftOperationResult (fun _ -> getAideAssetLookupRows csvPath)
            return! withTransaction <| sforMz rows insertAideAssetLookupRow
        }

