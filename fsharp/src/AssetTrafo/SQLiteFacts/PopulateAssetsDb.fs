// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.SQLiteFacts


module PopulateAssetsDb =

    open System.Text.RegularExpressions

    open FSharp.Data

    open AssetTrafo.Base.SqliteConn
    open AssetTrafo.Base.DbExportSchema

    
    let emptyIfNull (source : string) : string = 
        match source with
        | null | "NULL" -> ""
        | _ -> source
    
    let emptyIfNone (source : string option) : string = 
        match source with
        | Some str -> emptyIfNull str
        | None -> ""

    // ************************************************************************
    // S4 Flocs



    // ************************************************************************
    // S4 Equipment

    type S4EquipmentTable = 
        CsvProvider< Sample = @"G:\work\Projects\asset_sync\equipment_migration_s1.csv"
                   , PreferOptionals = true >
    
    type S4EquipmentRow = S4EquipmentTable.Row

    let getS4EquipmentRows (cvsPath : string) : S4EquipmentRow list = 
        let table = S4EquipmentTable.Load(uri = cvsPath) in Seq.toList table.Rows


    
    
    let makeS4EquipmentInsert (row : S4EquipmentRow) : string option = 
        match row.``400 S/4 Equip Reference``, row.``Migration Status (Y/N)`` with
        | Some(num), true -> 
            let line1 = "INSERT INTO s4_equipment (s4_ref, s4_name, aib_pli_code, category, obj_type, obj_class, s4_floc) "
            let line2 = 
                sprintf "VALUES(%i, '%s', '%s', '%s', '%s', '%s', '%s');" 
                        num 
                        (emptyIfNone row.``Equipment Description``)
                        (emptyIfNull row.``AI2 AIB Reference``)
                        (emptyIfNull row.Category)
                        (emptyIfNone row.``Object Type``)
                        (emptyIfNone row.Class)
                        (emptyIfNull row.``L6_Floc Code``)
            String.concat "\n" [line1; line2] |> Some
        | _,_ -> None
    
    
    let insertEquipmentRow (row : S4EquipmentRow) : SqliteConn<unit> = 
        match makeS4EquipmentInsert row with
        | Some statement -> 
            executeNonQuery statement |>> ignore
        | None -> mreturn ()
    
    
    
    let insertS4EquipmentRows (csvPath : string) : SqliteConn<unit> = 
        sqliteConn { 
            let! rows = liftOperation (fun _ -> getS4EquipmentRows csvPath)
            return! withTransaction <| forMz rows insertEquipmentRow
        }
        
    // ************************************************************************
    // aib flocs

    let makeAibFlocInsert (row : AibFlocRow) : string option = 
        match row.Category with 
        | "INSTALLATION" -> 
            let line1 = "INSERT INTO aib_installation (sai_ref, common_name, installation_type) "
            let line2 = 
                sprintf "VALUES('%s', '%s', '%s');" 
                        row.Reference 
                        (stringValue row.AssetName)
                        row.AssetCode
            String.concat "\n" [line1; line2] |> Some

        | "PROCESS GROUP" ->
            let line1 = "INSERT INTO aib_process_group (sai_ref, asset_name, asset_type, parent_ref) "
            let line2 = 
                sprintf "VALUES('%s', '%s', '%s', '%s');" 
                        row.Reference 
                        (stringValue row.AssetName)
                        row.AssetType
                        row.ParentRef
            String.concat "\n" [line1; line2] |> Some

        | "PROCESS" ->
            let line1 = "INSERT INTO aib_process (sai_ref, asset_name, asset_type, parent_ref) "
            let line2 = 
                sprintf "VALUES('%s', '%s', '%s', '%s');" 
                        row.Reference 
                        (stringValue row.AssetName)
                        row.AssetType
                        row.ParentRef
            String.concat "\n" [line1; line2] |> Some
        
        | "PLANT" ->
            let line1 = "INSERT INTO aib_plant (sai_ref, asset_name, asset_type, parent_ref) "
            let line2 = 
                sprintf "VALUES('%s', '%s', '%s', '%s');" 
                        row.Reference 
                        (stringValue row.AssetName)
                        row.AssetType
                        row.ParentRef
            String.concat "\n" [line1; line2] |> Some

        | "PLANT ITEM" ->
            let line1 = "INSERT INTO aib_plant_item (sai_ref, asset_name, asset_type, parent_ref) "
            let line2 = 
                sprintf "VALUES('%s', '%s', '%s', '%s');" 
                        row.Reference 
                        (stringValue row.AssetName)
                        row.AssetType
                        row.ParentRef
            String.concat "\n" [line1; line2] |> Some
        | _ -> None


    let insertAibFlocRow (row : AibFlocRow) : SqliteConn<unit> = 
        match makeAibFlocInsert row with
        | Some statement -> 
            executeNonQuery statement |>> ignore
        | None -> mreturn ()


    let insertAibFlocRows (csvPath : string) : SqliteConn<unit> = 
        sqliteConn { 
            let! rows = liftOperation (fun _ -> getAibFlocRows csvPath)
            return! withTransaction <| forMz rows insertAibFlocRow
        }

    // ************************************************************************
    // aib equipment

    let makeAibEquipmentInsert (row : AibEquipmentRow) : string option = 
        match row.Reference with 
        | null | "" -> None
        | _ -> 
            let line1 = "INSERT INTO aib_equipment (pli_ref, equipment_name, equipment_type, category, parent_ref) "
            let line2 = 
                sprintf "VALUES('%s', '%s', '%s', '%s', '%s');" 
                        row.Reference
                        (stringValue row.AssetName)
                        (stringValue row.AssetType)
                        row.Category
                        row.ParentRef
            String.concat "\n" [line1; line2] |> Some

    let insertAibEquipmentRow (row : AibEquipmentRow) : SqliteConn<unit> = 
        match makeAibEquipmentInsert row with
        | Some statement -> 
            executeNonQuery statement |>> ignore
        | None -> mreturn ()

    let insertAibEquipmentRows (csvPath : string) : SqliteConn<unit> = 
        sqliteConn { 
            let! rows = liftOperation (fun _ -> getAibEquipmentRows csvPath)
            return! withTransaction <| forMz rows insertAibEquipmentRow
        }

