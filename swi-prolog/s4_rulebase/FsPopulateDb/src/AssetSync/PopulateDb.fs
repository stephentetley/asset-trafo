// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync

module PopulateDb =
    
    open FSharp.Data

    open SLSqlite.Core

    type ErrMsg = string


    let flocLength (flocCode : string) : int = 
        let arr : string [] = flocCode.Split( [| '-' |])
        arr.Length
    type S4DataTable = 
        CsvProvider< Sample = @"G:\work\Projects\asset_sync\S4_Floc_Mapping_Site-A-Z_General_Structure_Initial.csv"
                   , MissingValues = @"#N/A,NULL"
                   , CacheRows = false
                   , PreferOptionals = true
                   , AssumeMissingValues = true
                   >

    type S4DataRow = S4DataTable.Row
    
    let getS4DataRows (cvsPath : string) : seq<S4DataRow> = 
        let table = S4DataTable.Load(uri = cvsPath) in table.Rows
    
    let getS4DataTable (cvsPath : string) : Result<S4DataTable, ErrMsg> =
        try 
            let table = S4DataTable.Load(uri = cvsPath) in Ok table
        with
        | excn -> Error excn.Message


    // S4 generic strategy...
    
    let insertS4RecordsGeneric (step : S4DataRow -> ('a * IndexedCommand) option)
                                (rows : seq<S4DataRow>) : SqliteDb<unit> = 
        let insertStep (found : Set<'a>) (row : S4DataRow) : SqliteDb<Set<'a>>= 
            match step row with
            | None -> mreturn found
            | Some (ix,cmd) ->
                if found.Contains ix then
                    mreturn found
                else 
                    executeNonQueryIndexed cmd >>. mreturn (Set.add ix found)
        withTransaction <| 
            (sfoldM insertStep Set.empty rows |>> ignore)

    /// S4 site (level 1)
    let s4SiteInsertStep (row : S4DataRow) : (string * IndexedCommand) option = 
        match row.L1_Site_Code with
        | None -> None
        | Some siteCode -> 
            let sql = 
                """
                INSERT INTO s4_site 
                (s4_floc, 
                site_name)
                VALUES (?,?);
                """
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam siteCode)
                    |> addParam (optionNull stringParam row.``S/4 Hana Floc Description``)
            Some (siteCode, cmd)


    /// S4 Function (level 2)
    let s4FunctionInsertStep (row : S4DataRow) : (string * IndexedCommand) option = 
        match row.``L2_Floc Code``, row.L2_Function, row.L1_Site_Code with
        | Some flocCode, Some funName, Some parent-> 
            let sql = 
                """
                INSERT INTO s4_function 
                (s4_floc, 
                function_name, 
                short_code,
                object_description,
                parent_floc)
                VALUES (?,?,?, ?,?);
                """
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (stringParam funName)
                    |> addParam (optionNull stringParam row.L2_Code)
                    |> addParam (optionNull stringParam row.``L2_Object Description``)
                    |> addParam (stringParam parent)
            Some (flocCode, cmd)
        | _, _, _-> None
        
    /// S4 Process Group (level 3)
    let s4ProcessGroupInsertStep (row : S4DataRow) : (string * IndexedCommand) option = 
        match row.``L3_Floc Code``, row.``L3_Process Group``, row.``L2_Floc Code`` with
        | Some flocCode, Some pgName, Some parent-> 
            let sql = 
                """
                INSERT INTO s4_process_group
                 (s4_floc, 
                 process_group_name, 
                 short_code,
                 object_description,
                 parent_floc)
                 VALUES (?,?,?, ?,?)
                 """
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (stringParam pgName)
                    |> addParam (optionNull stringParam row.L3_Code)
                    |> addParam (optionNull stringParam row.``L3_Object Description``)
                    |> addParam (stringParam parent)
            Some (flocCode, cmd)
        | _, _, _-> None

    /// S4 Process (level 4)
    let s4ProcessInsertStep (row : S4DataRow) : (string * IndexedCommand) option = 
        match row.``L4_Floc Code``, row.``L4_Process``, row.``L3_Floc Code`` with
        | Some flocCode, Some procName, Some parent-> 
            let sql = 
                """
                INSERT INTO s4_process 
                (s4_floc, 
                process_name, 
                short_code,
                object_description,
                parent_floc)
                VALUES (?,?,?, ?,?);
                """
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (stringParam procName)
                    |> addParam (optionNull stringParam row.L4_Code)
                    |> addParam (optionNull stringParam row.``L4_Object Description``)
                    |> addParam (stringParam parent)
            Some (flocCode, cmd)
        | _, _, _-> None

    /// S4 System (level 5)
    let s4SystemInsertStep (row : S4DataRow) : (string * IndexedCommand) option = 
        match row.``L5_Floc Code``, row.``L5_System``, row.``L4_Floc Code`` with
        | Some flocCode, Some procName, Some parent-> 
            let sql = 
                """
                INSERT INTO s4_system
                (s4_floc, 
                system_name, 
                short_code,
                object_code,
                object_description,
                class_code,
                class_description,
                parent_floc)
                VALUES (?,?,?, ?,?,?, ?,?);
                """
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (stringParam procName)
                    |> addParam (optionNull stringParam row.``L5_System Code``)
                    |> addParam (optionNull stringParam row.``L5_Object Type``)
                    |> addParam (optionNull stringParam row.``L5_Object Type Description``)
                    |> addParam (optionNull stringParam row.L5_Class)
                    |> addParam (optionNull stringParam row.``L5_Class Desc``)
                    |> addParam (stringParam parent)
            Some (flocCode, cmd)
        | _, _, _-> None

    /// S4 Assembly (level 6)
    let s4AssemblyInsertStep (row : S4DataRow) : (string * IndexedCommand) option = 
        match row.``L6_Floc Code``, row.``L6_Unit Description``, row.``L5_Floc Code`` with
        | Some flocCode, Some procName, Some parent 
            when flocLength flocCode = 6 -> 
            let sql = 
                """
                INSERT INTO s4_assembly
                (s4_floc, 
                assembly_name, 
                object_code,
                object_description,
                class_code,
                class_description,
                parent_floc)
                VALUES (?,?,?, ?,?,?, ?);
                """
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (stringParam procName)
                    |> addParam (optionNull stringParam row.``L6_Object Type``)
                    |> addParam (optionNull stringParam row.``L6_Object Type Description``)
                    |> addParam (optionNull stringParam row.L6_Class)
                    |> addParam (optionNull stringParam row.``L6_Class Desc``)
                    |> addParam (stringParam parent)
            Some (flocCode, cmd)
        | _, _, _-> None



    /// Insert all 'floc' assets
    let insertS4FlocRecords (csvPath : string) : SqliteDb<unit> = 
        let insertTraversal (step : S4DataRow -> (string * IndexedCommand) option) : SqliteDb<unit> = 
            sqliteDb { 
                let! table = 
                    liftOperationResult (fun _ -> getS4DataTable csvPath)
                return! insertS4RecordsGeneric step table.Rows
            }
        sqliteDb { 
            do! insertTraversal s4SiteInsertStep
            do! insertTraversal s4FunctionInsertStep
            do! insertTraversal s4ProcessGroupInsertStep
            do! insertTraversal s4ProcessInsertStep
            do! insertTraversal s4SystemInsertStep
            do! insertTraversal s4AssemblyInsertStep
            // Items and Components not currently represented in sample data
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


    /// Note - subordinate equipment not currently represented in the sample data
    let makeS4EquipmentInsert (row : S4EquipmentRow) : IndexedCommand option = 
        match row.``400 S/4 Equip Reference``, row.``Migration Status (Y/N)`` with
        | Some(num), true -> 
            let sql = 
                """
                INSERT INTO s4_equipment
                (s4_equip_ref, 
                description, 
                category,
                model,
                manufacturer,
                serial_number,
                object_type, 
                object_class, 
                s4_floc)
                VALUES (?,?,?, ?,?,?, ?,?,?)
                """
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (int32Param num)
                    |> addParam (optionNull stringParam row.``Equipment Description``)
                    |> addParam (stringParam row.Category)
                    |> addParam (optionNull stringParam row.``Std Manufacturer``)
                    |> addParam (optionNull stringParam row.``Std Model``)
                    |> addParam (optionNull stringParam row.``Std Serial``)
                    |> addParam (optionNull stringParam row.``Object Type``)
                    |> addParam (optionNull stringParam row.Class)
                    |> addParam (stringParam row.``L6_Floc Code``)
            cmd |> Some
        | _,_ -> None
    
   
    
    let insertS4EquipmentRecords (csvPath : string) : SqliteDb<unit> = 
        let insertEquipmentRow (row : S4EquipmentRow) : SqliteDb<unit> = 
            match makeS4EquipmentInsert row with
            | Some statement -> 
                executeNonQueryIndexed statement |>> ignore
            | None -> mreturn ()

        sqliteDb { 
            let! rows = liftOperation (fun _ -> getS4EquipmentRows csvPath)
            return! withTransaction <| forMz rows insertEquipmentRow
        }