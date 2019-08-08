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

    // This table is huge, but we process it with multiple traversals anyway
    // otherwise the processing is too complicated

    type S4FlocTable = 
        CsvProvider< Sample = @"G:\work\Projects\asset_sync\S4_Floc_Mapping_Site-A-Z_General_Structure_Initial.csv"
                   , MissingValues = @"#N/A,NULL"
                   , CacheRows = false
                   , PreferOptionals = true
                   , AssumeMissingValues = true
                   >
    
    type S4FlocRow = S4FlocTable.Row

    let getS4FlocRows (cvsPath : string) : seq<S4FlocRow> = 
        let table = S4FlocTable.Load(uri = cvsPath) in table.Rows



    let makeS4InstallationInsert (row : S4FlocRow) : string = 
        let code = Option.defaultValue "impossible" row.L1_Site_Code
        let line1 = "INSERT INTO s4_installation (s4_floc) "
        let line2 = 
            sprintf "VALUES('%s');" 
                    code 
        String.concat "\n" [line1; line2]
    
    let insertS4Installation (encountered : Set<string>) 
                                (row : S4FlocRow) : SqliteConn<Set<string>> = 
        match row.L1_Site_Code with
        | Some code -> 
            if not (encountered.Contains code) then
                let statement = makeS4InstallationInsert row
                executeNonQuery statement >>. mreturn (encountered.Add code)
            else
                mreturn encountered
        | _ -> mreturn encountered


    let insertS4InstallationRecords (rows : seq<S4FlocRow>) : SqliteConn<unit> = 
        withTransaction <| 
            (seqFoldM insertS4Installation Set.empty rows |>> ignore)
        

    let insertS4FlocRecords (csvPath : string) : SqliteConn<unit> = 
        sqliteConn { 
            let! rows = liftOperation (fun _ -> getS4FlocRows csvPath)
            do! insertS4InstallationRecords rows
            return ()
        }

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
    
    
    
    let insertS4EquipmentRecords (csvPath : string) : SqliteConn<unit> = 
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


    let insertAibFlocRecords (csvPath : string) : SqliteConn<unit> = 
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

    let insertAibEquipmentRecords (csvPath : string) : SqliteConn<unit> = 
        sqliteConn { 
            let! rows = liftOperation (fun _ -> getAibEquipmentRows csvPath)
            return! withTransaction <| forMz rows insertAibEquipmentRow
        }

