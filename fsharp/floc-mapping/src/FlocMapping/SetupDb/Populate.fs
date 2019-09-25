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
            common_name,
            short_code, 
            category, 
            asset_type, 
            parent_ref) 
            VALUES (?,?,?,  ?,?,?, ?);
            """
        new IndexedCommand(commandText = sql)
            |> addParam (stringParam row.Reference)
            |> addParam (stringParam row.AssetName)
            |> addParam (stringParam row.CommonName)
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
            common_name,
            category, 
            equipment_type, 
            parent_ref) 
            VALUES (?,?,?,  ?,?,?);
            """
        new IndexedCommand(commandText = sql)
            |> addParam (stringParam row.Reference)
            |> addParam (stringParam row.AssetName)
            |> addParam (stringParam row.CommonName)
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


    // ************************************************************************
    // Table: s4_floc

    let private s4FlocInsert (flocCode1 : string option) 
                                (name1 : string option)
                                (category1 : string option)
                                (parentFloc1 : string option) : (string * IndexedCommand) option =
        match flocCode1 with
        | None -> None
        | Some flocCode -> 
            let sql = 
                """
                INSERT INTO s4_floc
                 (s4_floc, name, category, parent_floc)
                 VALUES (?,?,?, ?);
                """
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (stringParam flocCode)
                    |> addParam (optionNull stringParam name1)
                    |> addParam (optionNull stringParam category1)
                    |> addParam (optionNull stringParam parentFloc1)
            Some (flocCode, cmd)

    let insertS4Site (found : Set<string>)
                     (row : S4FlocRow) : SqliteDb<Set<string>> = 
        match s4FlocInsert (row.L1_Site_Code) 
                           (row.``S/4 Hana Floc Description``)
                           (Some "SITE") 
                           None with
        | None -> mreturn found
        | Some (code, cmd) -> 
            if Set.contains code found then
                mreturn found
            else
                executeNonQueryIndexed cmd >>. mreturn (Set.add code found)
            
    let insertS4Function (found : Set<string>)
                     (row : S4FlocRow) : SqliteDb<Set<string>> = 
        match s4FlocInsert (row.``L2_Floc Code``) 
                              (row.L2_Function)
                              (Some "FUNCTION") 
                              (row.L1_Site_Code) with
        | None -> mreturn found
        | Some (code, cmd) -> 
            if Set.contains code found then
                mreturn found
            else
                executeNonQueryIndexed cmd >>. mreturn (Set.add code found)

    let insertS4ProcessGroup (found : Set<string>)
                             (row : S4FlocRow) : SqliteDb<Set<string>> = 
        match s4FlocInsert (row.``L3_Floc Code``) 
                              (row.``L3_Process Group``)
                              (Some "PROCESS GROUP") 
                              (row.``L2_Floc Code``) with
        | None -> mreturn found
        | Some (code, cmd) -> 
            if Set.contains code found then
                mreturn found
            else
                executeNonQueryIndexed cmd >>. mreturn (Set.add code found)


    let insertS4Process (found : Set<string>)
                        (row : S4FlocRow) : SqliteDb<Set<string>> = 
        match s4FlocInsert (row.``L4_Floc Code``) 
                            (row.``L4_Process``)
                            (Some "PROCESS") 
                            (row.``L3_Floc Code``) with
        | None -> mreturn found
        | Some (code, cmd) -> 
            if Set.contains code found then
                mreturn found
            else
                executeNonQueryIndexed cmd >>. mreturn (Set.add code found)
    
    let insertS4System (found : Set<string>)
                        (row : S4FlocRow) : SqliteDb<Set<string>> = 
        match s4FlocInsert (row.``L5_Floc Code``) 
                            (row.L5_System)
                            (Some "SYSTEM") 
                            (row.``L4_Floc Code``) with
        | None -> mreturn found
        | Some (code, cmd) -> 
            if Set.contains code found then
                mreturn found
            else
                executeNonQueryIndexed cmd >>. mreturn (Set.add code found)
    
    let insertS4Assembly (found : Set<string>)
                            (row : S4FlocRow) : SqliteDb<Set<string>> = 
        match s4FlocInsert (row.``L6_Floc Code``) 
                            (row.``L6_Unit Description``)
                            (Some "ASSEMBLY") 
                            (row.``L5_Floc Code``) with
        | None -> mreturn found
        | Some (code, cmd) -> 
            if Set.contains code found then
                mreturn found
            else
                executeNonQueryIndexed cmd >>. mreturn (Set.add code found)

    let insertS4Item (found : Set<string>)
                        (row : S4FlocRow) : SqliteDb<Set<string>> = 
        match s4FlocInsert (row.``L7_Floc Code``) 
                            (row.``L7_Sub Unit Description``)
                            (Some "ITEM") 
                            (row.``L6_Floc Code``) with
        | None -> mreturn found
        | Some (code, cmd) -> 
            if Set.contains code found then
                mreturn found
            else
                executeNonQueryIndexed cmd >>. mreturn (Set.add code found)
    
    let insertS4Component (found : Set<string>)
                (row : S4FlocRow) : SqliteDb<Set<string>> = 
        match s4FlocInsert (row.``L8_Floc Code``) 
                            (row.``L8_Item Description``)
                            (Some "COMPONENT") 
                            (row.``L7_Floc Code``) with
        | None -> mreturn found
        | Some (code, cmd) -> 
            if Set.contains code found then
                mreturn found
            else
                executeNonQueryIndexed cmd >>. mreturn (Set.add code found)

    let insertS4FlocRecords () : SqliteDb<unit> = 
        let insertStep (found : Set<string>) (row : S4FlocRow) : SqliteDb<Set<string>>= 
            sqliteDb { 
                let! s1 = insertS4Site found row
                let! s2 = insertS4Function s1 row
                let! s3 = insertS4ProcessGroup s2 row
                let! s4 = insertS4Process s3 row
                let! s5 = insertS4System s4 row
                let! s6 = insertS4Assembly s5 row
                let! s7 = insertS4Item s6 row
                let! s8 = insertS4Component s7 row
                return s8
            }
        sqliteDb { 
            let! table = liftOperationResult (fun _ -> readS4FlocTable ())
            return! withTransaction <| (sfoldM insertStep Set.empty table.Rows |>> ignore)
        }



    // ************************************************************************
    // Table: s4_equipment

    let makeS4EquipmentInsert (row : S4EquipmentRow) : IndexedCommand option = 
        match row.``400 S/4 Equip Reference``, row.``Migration Status (Y/N)`` with
        | Some(num), true -> 
            let sql = 
                """
                INSERT INTO s4_equipment
                (s4_ref, 
                name, 
                category,  
                obj_type, 
                obj_class, 
                s4_floc)
                VALUES (?,?,?, ?,?,?);
                """
            let cmd = 
                new IndexedCommand(commandText = sql)
                    |> addParam (int64Param num)
                    |> addParam (optionNull stringParam row.``Equipment Description``)
                    |> addParam (stringParam row.Category)
                    |> addParam (optionNull stringParam row.``Object Type``)
                    |> addParam (optionNull stringParam row.Class)
                    |> addParam (stringParam row.``L6_Floc Code``)
            cmd |> Some
        | _,_ -> None

    let insertS4EquipmentRecords () : SqliteDb<unit> = 
        let insertRow (row : S4EquipmentRow) : SqliteDb<unit> = 
            match makeS4EquipmentInsert row with
            | Some statement -> 
                executeNonQueryIndexed statement |>> ignore
            | None -> mreturn ()

        sqliteDb { 
            let! table = liftOperationResult (fun _ -> readS4EquipmentTable ())
            return! withTransaction <| smapMz insertRow table.Rows 
        }


    // ************************************************************************
    // Table: s4_aib_reference

    // MissingValues does not seem to recognize the string "NULL"
    // as None.
    let installationAibRef (row : S4FlocRow) : string option = 
        match (row.InstallationReference, row.SubInstallationReference) with
        // Prefer SubInst reference unless is is null
        | Some inst, None -> Some inst
        | Some inst, Some subInst when subInst = "NULL" -> Some inst
        | _, Some subInst when subInst <> "NULL" -> Some subInst
        | _,_ -> None

    type Link = string * string 
    
    
    let s4AibFlocLinks (links : Set<Link>) (row : S4FlocRow) : Set<Link> = 
        let addL1 x = 
            match installationAibRef row, row.L1_Site_Code with
            | Some sai, Some floc -> Set.add (sai, floc) x
            | _, _ -> x

        let addL3 x = 
            match row.ProcessGroupReference, row.``L3_Floc Code`` with
            | Some sai, Some floc when sai <> "NULL" && floc <> "NULL" -> 
                Set.add (sai, floc) x
            | _, _ -> x

        let addL4 x = 
            match row.ProcessReference, row.``L4_Floc Code`` with
            | Some sai, Some floc when sai <> "NULL" && floc <> "NULL" -> 
                Set.add (sai, floc) x
            | _, _ -> x

        // Are we sure this link is truthful?
        let addL6 x = 
            match row.PlantReference, row.``L6_Floc Code`` with
            | Some sai, Some floc when sai <> "NULL" && floc <> "NULL" -> 
                Set.add (sai, floc) x
            | _, _ -> x


        let addL6AIB x = 
            match row.L6_AIB_Reference, row.``L6_Floc Code`` with
            | Some sai, Some floc when sai <> "NULL" && floc <> "NULL" -> 
                Set.add (sai, floc) x
            | _, _ -> x
            
        let addL6Equipment x = 
            match row.``L6_Equipment PLI``, row.``L6_Floc Code`` with
            | Some sai, Some floc when sai <> "NULL" && floc <> "NULL" -> 
                Set.add (sai, floc) x
            | _, _ -> x

        let addL7AIB x = 
            match row.L7_AIB_Reference, row.``L7_Floc Code`` with
            | Some sai, Some floc when sai <> "NULL" && floc <> "NULL" -> 
                Set.add (sai, floc) x
            | _, _ -> x
    
        let addL7Equipment x = 
            match row.``L7_Equipment PLI``, row.``L7_Floc Code`` with
            | Some sai, Some floc when sai <> "NULL" && floc <> "NULL" -> 
                Set.add (sai, floc) x
            | _, _ -> x
        
        let addL8AIB x = 
            match row.``L8_AIB Reference``, row.``L8_Floc Code`` with
            | Some sai, Some floc when sai <> "NULL" && floc <> "NULL" -> 
                Set.add (sai, floc) x
            | _, _ -> x
    
        let addL8Equipment x = 
            match row.``L8_Equipment PLI``, row.``L8_Floc Code`` with
            | Some sai, Some floc when sai <> "NULL" && floc <> "NULL" -> 
                Set.add (sai, floc) x
            | _, _ -> x

        links 
            |> addL1    |> addL3 
            |> addL4    |> addL6
            |> addL6AIB |> addL6Equipment
            |> addL7AIB |> addL7Equipment    
            |> addL8AIB |> addL8Equipment
    
    let makeFlocLinkInsert (sai : string, floc: string) : IndexedCommand = 
        let sql = 
            """
            INSERT INTO aib_ref_to_s4_floc 
            (aib_ref, 
            s4_floc)
            VALUES (?,?);
            """
        new IndexedCommand(commandText = sql)
            |> addParam (stringParam sai)
            |> addParam (stringParam floc)

    let insertFlocLinks () : SqliteDb<unit> = 
        let insertLink (link : string * string) : SqliteDb<unit> = 
            let statement = makeFlocLinkInsert link
            executeNonQueryIndexed statement |>> ignore

        sqliteDb { 
            let! table = liftOperationResult (fun _ -> readS4FlocTable ()) 
            let links = Seq.fold s4AibFlocLinks Set.empty table.Rows |> Set.toList
            return! withTransaction <| forMz links insertLink
        }

    let makeEquipmentLinkInsert (pli : string) (equipNum: int64) : IndexedCommand = 
        let sql = 
            """
            INSERT INTO pli_ref_to_s4_equip 
            (pli_ref, 
            s4_equip)
            VALUES (?,?);
            """
        new IndexedCommand(commandText = sql)
            |> addParam (stringParam pli)
            |> addParam (int64Param equipNum)

    let insertEquipmentLinks () : SqliteDb<unit> = 
        let insertLink (row : S4EquipmentRow) : SqliteDb<unit> = 
            match row.``AI2 AIB Reference``, row.``400 S/4 Equip Reference`` with
            | pli, Some x -> 
                let statement = makeEquipmentLinkInsert pli (int64 x)
                executeNonQueryIndexed statement |>> ignore
            | _, _ -> mreturn ()

        sqliteDb { 
            let! table = liftOperationResult (fun _ -> readS4EquipmentTable ()) 
            return! withTransaction <| sforMz table.Rows insertLink
        }

