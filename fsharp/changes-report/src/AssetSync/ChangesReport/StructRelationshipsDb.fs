// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module StructRelationshipsDb =

    open System.IO
    open FSharp.Data

    open SLSqlite.Utils
    open SLSqlite.SqliteDb


    // Helper
    let insertStatement (tableName : string) (columnNames : string []) (values : string []) : string =
        let columns = String.concat ", " columnNames
        let vals = String.concat ", " values
        sprintf "INSERT INTO %s (%s)\nVALUES (%s);" tableName columns vals


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



    let makeStructRelInsert (row : AideStructureRelationshipRow) : string option =
        let columns =
            [| "structure_relationship_id"
            ; "parent_id"
            ; "child_id"
            |]
        insertStatement "aide_structure_relationships"
                        columns
                        [| row.StructureRelationshipId  |> fmtBigInt
                         ; row.ParentAideAssetId        |> fmtBigInt
                         ; row.ChildAideAssetId         |> fmtBigInt
                        |] |> Some


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
         AssetId(int64),\
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



    let makeAideAssetLookupInsert (row : AideAssetLookupRow) : string option =
        let columns =
            [| "aide_asset_id"
            ; "asset_id"
            ; "reference"
            ; "change_request_id"
            ; "asset_name"
            ; "asset_type"
            ; "asset_category"
            |]
        insertStatement "aide_asset_lookups"
                        columns
                        [| row.AideAssetId          |> fmtBigInt
                        ; row.AideAssetId           |> fmtBigInt
                        ; row.Reference             |> fmtText
                        ; row.ChangeRequestId       |> Option.defaultValue (-1L)  |> fmtBigInt
                        ; row.AssetName             |> fmtText
                        ; row.AssetType             |> fmtText
                        ; row.AssetCategory         |> fmtText
                        |] |> Some


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

