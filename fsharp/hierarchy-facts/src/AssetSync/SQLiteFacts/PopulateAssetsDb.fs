// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.SQLiteFacts


module PopulateAssetsDb =

    open System.Text.RegularExpressions

    open FSharp.Data

    open System.Data.SQLite

    open SLSqlite.Core

    // open AssetSync.Base.Addendum
    open AssetSync.Base.DbExportSchema



    // ************************************************************************
    // S4 Flocs

    // This table is huge, but we process it with multiple traversals as
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

    let getS4FlocTable (cvsPath : string) : Result<S4FlocTable, ErrMsg> =
        try 
            let table = S4FlocTable.Load(uri = cvsPath) in Ok table
        with
        | excn -> Error excn.Message




    // S4 generic strategy...
 
    let insertS4RecordsGeneric (step : S4FlocRow -> ('a * IndexedCommand) option)
                               (rows : seq<S4FlocRow>) : SqliteDb<unit> = 

        let insertStep (found : Set<'a>) (row : S4FlocRow) : SqliteDb<Set<'a>>= 
            match step row with
            | None -> mreturn found
            | Some (ix,cmd) ->
                if found.Contains ix then
                    mreturn found
                else 
                    executeNonQueryIndexed cmd >>. mreturn (Set.add ix found)
        withTransaction <| 
            (sfoldM insertStep Set.empty rows |>> ignore)

    let instAibRef (row : S4FlocRow) : SQLiteParameter = 
           match (row.InstallationReference, row.SubInstallationReference) with
           | Some inst, None -> stringParam inst
           | _, Some subInst  -> stringParam subInst
           | _,_ -> nullParam ()



    // S4 site (level 1)
    let s4SiteInsertStep (row : S4FlocRow) : (string * IndexedCommand) option = 
        match row.L1_Site_Code with
        | None -> None
        | Some siteCode -> 
            let sql = 
                "INSERT INTO s4_site (s4_floc, name, aib_ref) \
                VALUES (?,?,?)"
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam siteCode)
                    |> addParam (optionNull stringParam row.``S/4 Hana Floc Description``)
                    |> addParam (instAibRef row)
            Some (siteCode, cmd)

    




    //// S4 Function (level 2)
    let s4FunctionInsertStep (row : S4FlocRow) : (string * IndexedCommand) option = 
        match row.``L2_Floc Code`` with
        | None -> None
        | Some flocCode -> 
            let sql = 
                "INSERT INTO s4_function \
                 (s4_floc, name, aib_ref, parent_floc) \
                 VALUES (?,?,?,?)"
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (optionNull stringParam  row.L2_Function)
                    |> addParam (instAibRef row)
                    |> addParam (optionNull stringParam row.L1_Site_Code)
            Some (flocCode, cmd)




    //// S4 Process Group (level 3)
    let s4ProcessGroupInsertStep (row : S4FlocRow) : (string * IndexedCommand) option = 
        match row.``L3_Floc Code`` with
        | None -> None
        | Some flocCode -> 
            let sql = 
                "INSERT INTO s4_process_group \
                 (s4_floc, name, aib_ref, parent_floc) \
                 VALUES (?,?,?,?)"
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (optionNull stringParam  row.``L3_Process Group``)
                    |> addParam (instAibRef row)
                    |> addParam (optionNull stringParam row.``L2_Floc Code``)
            Some (flocCode, cmd)

    



    /// S4 Process (level 4)
    let s4ProcessInsertStep (row : S4FlocRow) : (string * IndexedCommand) option =
        match row.``L4_Floc Code`` with
        | None -> None
        | Some flocCode -> 
            let sql = 
                "INSERT INTO s4_process \
                 (s4_floc, name, aib_ref, parent_floc) \
                 VALUES (?,?,?,?)"
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (optionNull stringParam  row.L4_Process)
                    |> addParam (instAibRef row)
                    |> addParam (optionNull stringParam row.``L3_Floc Code``)
            Some (flocCode, cmd)
        

    /// S4 system (level 5)
    let s4SystemInsertStep (row : S4FlocRow) : (string * IndexedCommand) option =
        match row.``L5_Floc Code`` with
        | None -> None
        | Some flocCode -> 
            let sql = 
                "INSERT INTO s4_system \
                 (s4_floc, name, aib_ref, parent_floc) \
                 VALUES (?,?,?,?)"
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (optionNull stringParam  row.L5_System)
                    |> addParam (instAibRef row)
                    |> addParam (optionNull stringParam row.``L4_Floc Code``)
            Some (flocCode, cmd)

    

    // S4 assembly (level 6)
    let s4AssemblyInsertStep (row : S4FlocRow) : (string * IndexedCommand) option =
        match row.``L6_Unit Description`` with
        | None -> None
        | Some flocCode -> 
            let sql = 
                "INSERT INTO s4_assembly \
                 (s4_floc, name, aib_ref, parent_floc) \
                 VALUES (?,?,?,?)"
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (optionNull stringParam  row.``L6_Unit Description``)
                    |> addParam (instAibRef row)
                    |> addParam (optionNull stringParam row.``L5_Floc Code``)
            Some (flocCode, cmd)
    

    /// S4 item (level 7)
    let s4ItemInsertStep (row : S4FlocRow) : (string * IndexedCommand) option =
        match row.``L7_Sub Unit Description`` with
        | None -> None
        | Some flocCode -> 
            let sql = 
                "INSERT INTO s4_item \
                 (s4_floc, name, aib_ref, parent_floc) \
                 VALUES (?,?,?,?)"
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (optionNull stringParam  row.``L7_Sub Unit Description``)
                    |> addParam (instAibRef row)
                    |> addParam (optionNull stringParam row.``L6_Floc Code``)
            Some (flocCode, cmd)

    

    /// S4 component (level 6)
    let s4ComponentInsertStep (row : S4FlocRow) : (string * IndexedCommand) option =
        match row.``L8_Floc Code`` with
        | None -> None
        | Some flocCode -> 
            let sql = 
                "INSERT INTO s4_component \
                 (s4_floc, name, aib_ref, parent_floc) \
                 VALUES (?,?,?,?)"
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (optionNull stringParam  row.``L8_Item Description``)
                    |> addParam (instAibRef row)
                    |> addParam (optionNull stringParam row.``L7_Floc Code``)
            Some (flocCode, cmd)
  

    // Multiple traversal is simpler to think about, but 
    // we can't seem to traverse multiple times on table.Rows,
    // so we load the file each time.
    let insertS4FlocRecords (csvPath : string) : SqliteDb<unit> = 
        let insertTraversal (step : S4FlocRow -> (string * IndexedCommand) option) : SqliteDb<unit> = 
            sqliteDb { 
                let! table = 
                    liftOperationResult (fun _ -> getS4FlocTable csvPath)
                return! insertS4RecordsGeneric step table.Rows
            }
        sqliteDb { 
            do! insertTraversal s4SiteInsertStep
            do! insertTraversal s4FunctionInsertStep
            do! insertTraversal s4ProcessGroupInsertStep
            do! insertTraversal s4ProcessInsertStep
            do! insertTraversal s4SystemInsertStep
            do! insertTraversal s4AssemblyInsertStep
            do! insertTraversal s4ItemInsertStep
            do! insertTraversal s4ComponentInsertStep
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


    
    let makeS4EquipmentInsert (row : S4EquipmentRow) : IndexedCommand option = 
        match row.``400 S/4 Equip Reference``, row.``Migration Status (Y/N)`` with
        | Some(num), true -> 
            let sql = 
                "INSERT INTO s4_equipment \
                (s4_ref, name, aib_pli_code, category, obj_type, obj_class, s4_floc) \
                VALUES (?,?,?, ?,?,?, ?)"
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (int32Param num)
                    |> addParam (optionNull stringParam row.``Equipment Description``)
                    |> addParam (stringParam row.``AI2 AIB Reference``)
                    |> addParam (stringParam row.Category)
                    |> addParam (optionNull stringParam row.``Object Type``)
                    |> addParam (optionNull stringParam row.Class)
                    |> addParam (stringParam row.``L6_Floc Code``)
            cmd |> Some
        | _,_ -> None
    
    
    let insertEquipmentRow (row : S4EquipmentRow) : SqliteDb<unit> = 
        match makeS4EquipmentInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()
    
    
    
    let insertS4EquipmentRecords (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! rows = liftOperation (fun _ -> getS4EquipmentRows csvPath)
            return! withTransaction <| forMz rows insertEquipmentRow
        }
        
    // ************************************************************************
    // aib flocs

    let makeAibFlocInsert (row : AibFlocRow) : IndexedCommand option = 
        match row.Category with 
        | "INSTALLATION" -> 
            let sql = 
                "INSERT INTO aib_installation \
                (sai_ref, common_name, installation_type) \
                VALUES (?,?,?)"
            let cmd = 
                new IndexedCommand (commandText = sql)
                    |> addParam (stringParam row.Reference)
                    |> addParam (stringParam row.AssetName)
                    |> addParam (stringParam row.AssetCode)
            cmd |> Some

        | "PROCESS GROUP" ->
            let sql = 
                "INSERT INTO aib_process_group \
                (sai_ref, asset_name, asset_type, parent_ref) \
                VALUES (?,?,?, ?)"
            let cmd = 
                new IndexedCommand (commandText = sql)
                    |> addParam (stringParam row.Reference)
                    |> addParam (stringParam row.AssetName)
                    |> addParam (stringParam row.AssetType)
                    |> addParam (stringParam row.ParentRef)
            cmd |> Some

        | "PROCESS" ->
            let sql = 
                "INSERT INTO aib_process \
                (sai_ref, asset_name, asset_type, parent_ref) \
                VALUES (?,?,?, ?)"
            let cmd = 
                 new IndexedCommand (commandText = sql)
                     |> addParam (stringParam row.Reference)
                     |> addParam (stringParam row.AssetName)
                     |> addParam (stringParam row.AssetType)
                     |> addParam (stringParam row.ParentRef)
            cmd |> Some
        
        | "PLANT" ->
            let sql = 
                "INSERT INTO aib_plant \
                (sai_ref, asset_name, asset_type, parent_ref) \
                VALUES (?,?,?, ?)"
            let cmd = 
                new IndexedCommand (commandText = sql)
                    |> addParam (stringParam row.Reference)
                    |> addParam (stringParam row.AssetName)
                    |> addParam (stringParam row.AssetType)
                    |> addParam (stringParam row.ParentRef)
            cmd |> Some

        | "PLANT ITEM" ->
            let sql = 
                "INSERT INTO aib_plant_item \
                (sai_ref, asset_name, asset_type, parent_ref) \
                VALUES(?,?,?, ?)"
            let cmd = 
                new IndexedCommand (commandText = sql)
                    |> addParam (stringParam row.Reference)
                    |> addParam (stringParam row.AssetName)
                    |> addParam (stringParam row.AssetType)
                    |> addParam (stringParam row.ParentRef)
            cmd |> Some
        | _ -> None


    let insertAibFlocRow (row : AibFlocRow) : SqliteDb<unit> = 
        match makeAibFlocInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()


    let insertAibFlocRecords (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! rows = liftOperation (fun _ -> getAibFlocRows csvPath)
            return! withTransaction <| forMz rows insertAibFlocRow
        }

    // ************************************************************************
    // aib equipment

    let makeAibEquipmentInsert (row : AibEquipmentRow) : IndexedCommand option = 
        match row.Reference with 
        | null | "" -> None
        | _ -> 
            let sql = 
                "INSERT INTO aib_equipment \
                (pli_ref, equipment_name, equipment_type, category, parent_ref) \
                VALUES (?,?,?,  ?,?)"
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam row.Reference)
                    |> addParam (stringParam row.AssetName)
                    |> addParam (stringParam row.AssetType)
                    |> addParam (stringParam row.Category)
                    |> addParam (stringParam row.ParentRef)
            cmd |> Some

    let insertAibEquipmentRow (row : AibEquipmentRow) : SqliteDb<unit> = 
        match makeAibEquipmentInsert row with
        | Some statement -> 
            executeNonQueryIndexed statement |>> ignore
        | None -> mreturn ()

    let insertAibEquipmentRecords (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! rows = liftOperation (fun _ -> getAibEquipmentRows csvPath)
            return! withTransaction <| forMz rows insertAibEquipmentRow
        }

