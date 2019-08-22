// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.StructureRelationships


module PopulateDb =

    open FSharp.Data
    
    open SLSqlite.Core

        

    // ************************************************************************
    // aide_structure_relationships table

    [<Literal>]
    let AideStructureRelationshipSchema = 
        "StructureRelationshipId(int64),\
         ParentAideAssetId(int64),\
         ChildAideAssetId(int64)"    



    type AideStructureRelationshipTable = 
        CsvProvider< Schema = AideStructureRelationshipSchema 
                   , Sample = AideStructureRelationshipSchema
                   , HasHeaders = true >

    type AideStructureRelationshipRow = AideStructureRelationshipTable.Row

    let getAideStructureRelationshipRows (cvsPath : string) : Result<seq<AideStructureRelationshipRow>, string> = 
        try
            let table = AideStructureRelationshipTable.Load(uri = cvsPath)
            table.Rows |> Ok
        with
        | excn -> Error excn.Message



    let makeAideStructRelInsert (row : AideStructureRelationshipRow) : KeyedCommand option =
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
        match makeAideStructRelInsert row with
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
         ChangeRequestId(int64 option),\
         AssetId(int64 option),\
         Reference(string),\
         AssetCommonName(string),\
         AssetName(string),\
         AssetType(string),\
         AssetCategory(string)"    



    type AideAssetLookupTable = 
        CsvProvider< Schema = AideAssetLookupSchema 
                   , Sample = AideAssetLookupSchema
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
            change_request_id, \
            asset_id, \
            reference, \
            asset_common_name, \
            asset_name, \
            asset_type, \
            asset_category) \
            VALUES \
            (?,?,?,  ?,?,?, ?,?)"
        let cmd = 
            new IndexedCommand (commandText = sql)
                |> addParam (int64Param row.AideAssetId)
                |> addParam (optionNull int64Param row.ChangeRequestId)
                |> addParam (optionNull int64Param row.AssetId)
                |> addParam (stringParam row.Reference)                
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

    // ************************************************************************
    // ai_structure_relationships table

    [<Literal>]
    let AiStructureRelationshipSchema = 
        "StructureRelationshipId(int64),\
         ParentAssetId(int64),\
         ChildAssetId(int64)"    



    type AiStructureRelationshipTable = 
        CsvProvider< Schema = AiStructureRelationshipSchema 
                   , Sample = AiStructureRelationshipSchema
                   , HasHeaders = true >

    type AiStructureRelationshipRow = AiStructureRelationshipTable.Row

    let getAiStructureRelationshipRows (cvsPath : string) : Result<seq<AiStructureRelationshipRow>, string> = 
        try
            let table = AiStructureRelationshipTable.Load(uri = cvsPath)
            table.Rows |> Ok
        with
        | excn -> Error excn.Message



    let makeAiStructRelInsert (row : AiStructureRelationshipRow) : KeyedCommand option =
        let sql = 
            "INSERT INTO ai_structure_relationships (structure_relationship_id, parent_id, child_id) \
            VALUES (:relid, :parent, :child)"
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "relid" (int64Param row.StructureRelationshipId)
                |> addNamedParam "parent" (int64Param row.ParentAssetId)
                |> addNamedParam "child"(int64Param row.ChildAssetId)
        cmd |> Some


    let insertAiStructRelationshipRow (row : AiStructureRelationshipRow) : SqliteDb<unit> = 
        match makeAiStructRelInsert row with
        | Some statement -> 
            executeNonQueryKeyed statement |>> ignore
        | None -> mreturn ()

    let insertAiStructRelationshipRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! rows = 
                liftOperationResult (fun _ -> getAiStructureRelationshipRows csvPath)
            return! withTransaction <| sforMz rows insertAiStructRelationshipRow
        }


    // ************************************************************************
    // ai_asset_lookups table

    [<Literal>]
    let AiAssetLookupSchema = 
         "AssetId(int64 option),\
         Reference(string),\
         AssetCommonName(string),\
         AssetName(string),\
         AssetType(string),\
         AssetCategory(string)"    



    type AiAssetLookupTable = 
        CsvProvider< Schema = AiAssetLookupSchema 
                   , Sample = AiAssetLookupSchema
                   , HasHeaders = true >

    type AiAssetLookupRow = AiAssetLookupTable.Row

    let getAiAssetLookupRows (cvsPath : string) : Result<seq<AiAssetLookupRow>, string> = 
        try
            let table = AiAssetLookupTable.Load(uri = cvsPath)
            table.Rows |> Ok
        with
        | excn -> Error excn.Message



    let makeAiAssetLookupInsert (row : AiAssetLookupRow) : IndexedCommand option =
        let sql = 
            "INSERT INTO aide_asset_lookups \
            (asset_id, \
            reference, \
            asset_common_name, \
            asset_name, \
            asset_type, \
            asset_category) \
            VALUES \
            (?,?,?,  ?,?,?, ?,?)"
        let cmd = 
            new IndexedCommand (commandText = sql)
                |> addParam (optionNull int64Param row.AssetId)
                |> addParam (stringParam row.Reference)              
                |> addParam (stringParam row.AssetCommonName)
                |> addParam (stringParam row.AssetName)
                |> addParam (stringParam row.AssetType)
                |> addParam (stringParam row.AssetCategory)
        cmd |> Some


    let insertAiAssetLookupRow (row : AiAssetLookupRow) : SqliteDb<unit> = 
        match makeAiAssetLookupInsert row with
        | Some statement -> 
            attempt (executeNonQueryIndexed statement)
                    (fun _msg -> throwError (sprintf "Bad Row %O" row)) |>> ignore
        | None -> mreturn ()

    let insertAiAssetLookupRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! rows = 
                liftOperationResult (fun _ -> getAiAssetLookupRows csvPath)
            return! withTransaction <| sforMz rows insertAiAssetLookupRow
        }

