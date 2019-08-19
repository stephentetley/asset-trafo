// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.SQLiteFacts


module PopulateAssetsDb =

    open System.Text.RegularExpressions

    open FSharp.Data

    open System.Data.SQLite

    open SLSqlite.Utils
    open SLSqlite.SqliteDb

    open AssetSync.Base.Addendum
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

    


    let insertS4SiteRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> getS4FlocTable csvPath)
            return! insertS4RecordsGeneric s4SiteInsertStep table.Rows
        }



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

    let insertS4FunctionRows (csvPath : string) : SqliteDb<unit> = 
        sqliteDb { 
            let! table = 
                liftOperationResult (fun _ -> getS4FlocTable csvPath)
            return! insertS4RecordsGeneric s4FunctionInsertStep table.Rows
        }

    //    //{ TableName = "s4_function"
    //    //  ColumnNames = ["s4_floc"; "name"; "aib_ref"; "parent_floc"]
    //    //  GetUniqueKeyForValidRow = 
    //    //    fun row -> 
    //    //        match row.``L2_Floc Code``, row.L2_Function with
    //    //        | Some code, Some _ -> Some code
    //    //        | _ , _  -> None
    //    //  MakeValues = 
    //    //    fun row -> 
    //    //        sprintf "'%s', '%s', '%s', '%s'" 
    //    //                (Option.defaultValue "impossible" row.``L2_Floc Code``) 
    //    //                (Option.defaultValue "impossible" row.L2_Function)
    //    //                (instAibRef row)        // same as site
    //    //                (Option.defaultValue "" row.L1_Site_Code) 
    //    //}


    //// S4 Process Group (level 3)
    //let s4StrategyProcessGroup : S4Strategy = 
    //    { TableName = "s4_process_group"
    //      ColumnNames = ["s4_floc"; "name"; "aib_ref"; "parent_floc"]
    //      GetUniqueKeyForValidRow = 
    //        fun row -> 
    //            match row.``L3_Floc Code``, row.``L3_Process Group`` with
    //            | Some code, Some _ -> Some code
    //            | _ , _  -> None
    //      MakeValues = 
    //        fun row -> 
    //            sprintf "'%s', '%s', '%s', '%s'" 
    //                    (Option.defaultValue "impossible" row.``L3_Floc Code``) 
    //                    (Option.defaultValue "impossible" row.``L3_Process Group``)
    //                    (emptyIfNone row.ProcessGroupReference)
    //                    (Option.defaultValue "" row.``L2_Floc Code``) 
    //    }



    //// S4 Process (level 4)
    //let s4StrategyProcess : S4Strategy = 
    //    { TableName = "s4_process"
    //      ColumnNames = ["s4_floc"; "name"; "aib_ref"; "parent_floc"]
    //      GetUniqueKeyForValidRow = 
    //        fun row -> 
    //            match row.``L4_Floc Code``, row.L4_Process with
    //            | Some code, Some _ -> Some code
    //            | _ , _  -> None
    //      MakeValues = 
    //        fun row -> 
    //            sprintf "'%s', '%s', '%s', '%s'" 
    //                    (Option.defaultValue "impossible" row.``L4_Floc Code``) 
    //                    (Option.defaultValue "impossible" row.L4_Process)
    //                    (emptyIfNone row.ProcessReference)
    //                    (Option.defaultValue "" row.``L3_Floc Code``) 
    //    }

    //// S4 system (level 5)
    //let s4StrategySystem : S4Strategy = 
    //    { TableName = "s4_system"
    //      ColumnNames = ["s4_floc"; "name"; "aib_ref"; "parent_floc"]
    //      GetUniqueKeyForValidRow = 
    //        fun row -> 
    //            match row.``L5_Floc Code``, row.L5_System with
    //            | Some code, Some _ -> Some code
    //            | _ , _  -> None
    //      MakeValues = 
    //        fun row -> 
    //            sprintf "'%s', '%s', '%s', '%s'" 
    //                    (Option.defaultValue "impossible" row.``L5_Floc Code``) 
    //                    (Option.defaultValue "impossible" row.L5_System)
    //                    (emptyIfNone row.ProcessReference)  // reference to process
    //                    (Option.defaultValue "" row.``L4_Floc Code``) 
    //    }

    //// S4 assembly (level 6)
    //let s4StrategyAssembly : S4Strategy = 
    //    { TableName = "s4_assembly"
    //      ColumnNames = ["s4_floc"; "name"; "aib_ref"; "parent_floc"]
    //      GetUniqueKeyForValidRow = 
    //        fun row -> 
    //            match row.``L6_Floc Code``, row.``L6_Unit Description`` with
    //            | Some code, Some _ -> Some code
    //            | _ , _  -> None
    //      MakeValues = 
    //        fun row -> 
    //            sprintf "'%s', '%s', '%s', '%s'" 
    //                    (Option.defaultValue "impossible" row.``L6_Floc Code``) 
    //                    (Option.defaultValue "impossible" row.``L6_Unit Description``)
    //                    (emptyIfNone row.PlantReference) 
    //                    (Option.defaultValue "" row.``L5_Floc Code``) 
    //    }

    //// S4 item (level 7)
    //let s4StrategyItem : S4Strategy = 
    //    { TableName = "s4_item"
    //      ColumnNames = ["s4_floc"; "name"; "aib_ref"; "parent_floc"]
    //      GetUniqueKeyForValidRow = 
    //        fun row -> 
    //            match row.``L7_Floc Code``, row.``L7_Sub Unit Description`` with
    //            | Some code, Some _ -> Some code
    //            | _ , _  -> None
    //      MakeValues = 
    //        fun row -> 
    //            sprintf "'%s', '%s', '%s', '%s'" 
    //                    (Option.defaultValue "impossible" row.``L7_Floc Code``) 
    //                    (Option.defaultValue "impossible" row.``L7_Sub Unit Description``)
    //                    (emptyIfNone row.PlantReference)        // Should this be the same as parent?
    //                    (Option.defaultValue "" row.``L6_Floc Code``) 
    //    }

    //// S4 component (level 6)
    //let s4StrategyComponent : S4Strategy = 
    //    { TableName = "s4_component"
    //      ColumnNames = ["s4_floc"; "name"; "aib_ref"; "parent_floc"]
    //      GetUniqueKeyForValidRow = 
    //        fun row -> 
    //            match row.``L8_Floc Code``, row.``L8_Item Description`` with
    //            | Some code, Some _ -> Some code
    //            | _ , _  -> None
    //      MakeValues = 
    //        fun row -> 
    //            sprintf "'%s', '%s', '%s', '%s'" 
    //                    (Option.defaultValue "impossible" row.``L8_Floc Code``) 
    //                    (Option.defaultValue "impossible" row.``L8_Item Description``)
    //                    (emptyIfNone row.PlantReference) 
    //                    (Option.defaultValue "" row.``L7_Floc Code``) 
    //    }

    //// Multiple traversal is simpler to think about, but 
    //// we can't see to travers multiple times on table.Rows,
    //// so we load the file each time.
    //let insertS4FlocRecords (csvPath : string) : SqliteDb<unit> = 
    //    let insertRecords (strategy : S4Strategy) = 
    //        liftOperation (fun _ -> getS4FlocTable csvPath) >>= 
    //        fun table -> insertS4RecordsGeneric strategy table.Rows
    //    sqliteDb { 
    //        do! insertRecords s4StrategySite
    //        do! insertRecords s4StrategyFunction
    //        do! insertRecords s4StrategyProcessGroup
    //        do! insertRecords s4StrategyProcess
    //        do! insertRecords s4StrategySystem
    //        do! insertRecords s4StrategyAssembly
    //        do! insertRecords s4StrategyComponent
    //        do! insertRecords s4StrategyItem
    //        return ()
    //    }



    //// ************************************************************************
    //// S4 Equipment

    //type S4EquipmentTable = 
    //    CsvProvider< Sample = @"G:\work\Projects\asset_sync\equipment_migration_s1.csv"
    //               , PreferOptionals = true >
    
    //type S4EquipmentRow = S4EquipmentTable.Row

    //let getS4EquipmentRows (cvsPath : string) : S4EquipmentRow list = 
    //    let table = S4EquipmentTable.Load(uri = cvsPath) in Seq.toList table.Rows


    
    //let makeS4EquipmentInsert (row : S4EquipmentRow) : string option = 
    //    match row.``400 S/4 Equip Reference``, row.``Migration Status (Y/N)`` with
    //    | Some(num), true -> 
    //        let line1 = "INSERT INTO s4_equipment (s4_ref, name, aib_pli_code, category, obj_type, obj_class, s4_floc) "
    //        let line2 = 
    //            sprintf "VALUES(%i, '%s', '%s', '%s', '%s', '%s', '%s');" 
    //                    num 
    //                    (emptyIfNone row.``Equipment Description``)
    //                    (emptyIfNull row.``AI2 AIB Reference``)
    //                    (emptyIfNull row.Category)
    //                    (emptyIfNone row.``Object Type``)
    //                    (emptyIfNone row.Class)
    //                    (emptyIfNull row.``L6_Floc Code``)
    //        String.concat "\n" [line1; line2] |> Some
    //    | _,_ -> None
    
    
    //let insertEquipmentRow (row : S4EquipmentRow) : SqliteDb<unit> = 
    //    match makeS4EquipmentInsert row with
    //    | Some statement -> 
    //        executeNonQuery statement |>> ignore
    //    | None -> mreturn ()
    
    
    
    //let insertS4EquipmentRecords (csvPath : string) : SqliteDb<unit> = 
    //    sqliteDb { 
    //        let! rows = liftOperation (fun _ -> getS4EquipmentRows csvPath)
    //        return! withTransaction <| forMz rows insertEquipmentRow
    //    }
        
    //// ************************************************************************
    //// aib flocs

    //let makeAibFlocInsert (row : AibFlocRow) : string option = 
    //    match row.Category with 
    //    | "INSTALLATION" -> 
    //        let line1 = "INSERT INTO aib_installation (sai_ref, common_name, installation_type) "
    //        let line2 = 
    //            sprintf "VALUES('%s', '%s', '%s');" 
    //                    row.Reference 
    //                    (stringValue row.AssetName)
    //                    row.AssetCode
    //        String.concat "\n" [line1; line2] |> Some

    //    | "PROCESS GROUP" ->
    //        let line1 = "INSERT INTO aib_process_group (sai_ref, asset_name, asset_type, parent_ref) "
    //        let line2 = 
    //            sprintf "VALUES('%s', '%s', '%s', '%s');" 
    //                    row.Reference 
    //                    (stringValue row.AssetName)
    //                    row.AssetType
    //                    row.ParentRef
    //        String.concat "\n" [line1; line2] |> Some

    //    | "PROCESS" ->
    //        let line1 = "INSERT INTO aib_process (sai_ref, asset_name, asset_type, parent_ref) "
    //        let line2 = 
    //            sprintf "VALUES('%s', '%s', '%s', '%s');" 
    //                    row.Reference 
    //                    (stringValue row.AssetName)
    //                    row.AssetType
    //                    row.ParentRef
    //        String.concat "\n" [line1; line2] |> Some
        
    //    | "PLANT" ->
    //        let line1 = "INSERT INTO aib_plant (sai_ref, asset_name, asset_type, parent_ref) "
    //        let line2 = 
    //            sprintf "VALUES('%s', '%s', '%s', '%s');" 
    //                    row.Reference 
    //                    (stringValue row.AssetName)
    //                    row.AssetType
    //                    row.ParentRef
    //        String.concat "\n" [line1; line2] |> Some

    //    | "PLANT ITEM" ->
    //        let line1 = "INSERT INTO aib_plant_item (sai_ref, asset_name, asset_type, parent_ref) "
    //        let line2 = 
    //            sprintf "VALUES('%s', '%s', '%s', '%s');" 
    //                    row.Reference 
    //                    (stringValue row.AssetName)
    //                    row.AssetType
    //                    row.ParentRef
    //        String.concat "\n" [line1; line2] |> Some
    //    | _ -> None


    //let insertAibFlocRow (row : AibFlocRow) : SqliteDb<unit> = 
    //    match makeAibFlocInsert row with
    //    | Some statement -> 
    //        executeNonQuery statement |>> ignore
    //    | None -> mreturn ()


    //let insertAibFlocRecords (csvPath : string) : SqliteDb<unit> = 
    //    sqliteDb { 
    //        let! rows = liftOperation (fun _ -> getAibFlocRows csvPath)
    //        return! withTransaction <| forMz rows insertAibFlocRow
    //    }

    //// ************************************************************************
    //// aib equipment

    //let makeAibEquipmentInsert (row : AibEquipmentRow) : string option = 
    //    match row.Reference with 
    //    | null | "" -> None
    //    | _ -> 
    //        let line1 = "INSERT INTO aib_equipment (pli_ref, equipment_name, equipment_type, category, parent_ref) "
    //        let line2 = 
    //            sprintf "VALUES('%s', '%s', '%s', '%s', '%s');" 
    //                    row.Reference
    //                    (stringValue row.AssetName)
    //                    (stringValue row.AssetType)
    //                    row.Category
    //                    row.ParentRef
    //        String.concat "\n" [line1; line2] |> Some

    //let insertAibEquipmentRow (row : AibEquipmentRow) : SqliteDb<unit> = 
    //    match makeAibEquipmentInsert row with
    //    | Some statement -> 
    //        executeNonQuery statement |>> ignore
    //    | None -> mreturn ()

    //let insertAibEquipmentRecords (csvPath : string) : SqliteDb<unit> = 
    //    sqliteDb { 
    //        let! rows = liftOperation (fun _ -> getAibEquipmentRows csvPath)
    //        return! withTransaction <| forMz rows insertAibEquipmentRow
    //    }

