// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module StructRelationshipsDb =

    open System.IO
    open FSharp.Data

    open System.Data.SQLite

    open SLSqlite.Core

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



    let makeStructRelInsert (row : AideStructureRelationshipRow) : KeyedCommand option =
        let sql = 
            "INSERT INTO aide_structure_relationships (structure_relationship_id, parent_id, child_id) \
            VALUES (:relid, :parent, :child)"
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "relid" (int64Param row.StructureRelationshipId)
                |> addNamedParam "parent" (int64Param row.ParentAideAssetId)
                |> addNamedParam "child"(int64Param row.ChildAideAssetId)
        cmd |> Some


    let insertAideStructRelationshipRow (row : AideStructureRelationshipRow) : SqliteDb<unit> = 
        match makeStructRelInsert row with
        | Some statement -> 
            executeNonQueryKeyed statement |>> ignore
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
         AssetCommonName(string),\
         AssetName(string),\
         AssetType(string),\
         AssetCategory(string)"    

    [<Literal>]
    let AideAssetLookupSample = 
        "1000,600000,PLI001,NULL,Asset Common Name,Asset Name, Asset Type, Asset Category"
       

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



    let makeAideAssetLookupInsert (row : AideAssetLookupRow) : IndexedCommand option =
        let sql = 
            "INSERT INTO aide_asset_lookups \
            (aide_asset_id, \
            asset_id, \
            reference, \
            change_request_id, \
            asset_common_name, \
            asset_name, \
            asset_type, \
            asset_category) \
            VALUES \
            (?,?,?,  ?,?,?, ?,?)"
        let cmd = 
            new IndexedCommand (commandText = sql)
                |> addParam (int64Param row.AideAssetId)
                |> addParam (optionNull int64Param row.AssetId)
                |> addParam (stringParam row.Reference)
                |> addParam (optionNull int64Param row.ChangeRequestId)                
                |> addParam (stringParam row.AssetCommonName)
                |> addParam (stringParam row.AssetName)
                |> addParam (stringParam row.AssetType)
                |> addParam (stringParam row.AssetCategory)
        cmd |> Some


    let insertAideAssetLookupRow (row : AideAssetLookupRow) : SqliteDb<unit> = 
        match makeAideAssetLookupInsert row with
        | Some statement -> 
            attempt (executeNonQueryIndexed statement)
                    (fun _msg -> throwError (sprintf "Bad Row %O" row)) |>> ignore
        | None -> mreturn ()

    let insertAideAssetLookupRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! rows = 
                liftOperationResult (fun _ -> getAideAssetLookupRows csvPath)
            return! withTransaction <| sforMz rows insertAideAssetLookupRow
        }

