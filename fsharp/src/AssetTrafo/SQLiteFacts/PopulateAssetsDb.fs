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
            let line1 = "INSERT INTO s4_equipment (s4_ref, pli_code, s4_name, category, obj_type, obj_class, s4_floc) "
            let line2 = 
                sprintf "VALUES(%i, '%s', '%s', '%s', '%s', '%s', '%s');" 
                        num 
                        (emptyIfNull row.``AI2 AIB Reference``)
                        (emptyIfNone row.``Equipment Description``)
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
    // aib installation

    let makeAibInstallationInsert (row : AibFlocRow) : string option = 
        if row.Category = "INSTALLATION" then
            let line1 = "INSERT INTO aib_installation (sai_ref, common_name, installation_type) "
            let line2 = 
                sprintf "VALUES('%s', '%s', '%s');" 
                        row.Reference 
                        row.AssetName
                        row.AssetCode
            String.concat "\n" [line1; line2] |> Some
        else None


   
    let insertAibInstallationRow (row : AibFlocRow) : SqliteConn<unit> = 
        match makeAibInstallationInsert row with
        | Some statement -> 
            executeNonQuery statement |>> ignore
        | None -> mreturn ()


    let insertAibRows (csvPath : string) : SqliteConn<unit> = 
        sqliteConn { 
            let! rows = liftOperation (fun _ -> getAibFlocRows csvPath)
            return! withTransaction <| forMz rows insertAibInstallationRow
        }


    //// aib_installation(ref, name, type).
    //let makeInstallationFact (namePattern : string) (row : FlocRow) : Predicate option = 
    //     if row.Category = "INSTALLATION" 
    //            && Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
    //         predicate "aib_installation" 
    //                     [ quotedAtom row.Reference
    //                     ; quotedAtom row.AssetName
    //                     ; quotedAtom row.AssetCode
    //                     ] |> Some
    //     else None

    //// aib_process_group(ref, name, type, parent).
    //let makeProcessGroupFact (namePattern : string) (row : FlocRow) : Predicate option = 
    //     if row.Category = "PROCESS GROUP" 
    //            && Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
    //         predicate "aib_process_group" 
    //                     [ quotedAtom row.Reference
    //                     ; quotedAtom row.AssetName
    //                     ; quotedAtom row.AssetType
    //                     ; quotedAtom row.ParentRef
    //                     ] |> Some
    //     else None

    //// aib_process(ref, name, type, parent).
    //let makeProcessFact (namePattern : string) (row : FlocRow) : Predicate option = 
    //     if row.Category = "PROCESS" 
    //            && Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
    //         predicate "aib_process" 
    //                     [ quotedAtom row.Reference
    //                     ; quotedAtom row.AssetName
    //                     ; quotedAtom row.AssetType
    //                     ; quotedAtom row.ParentRef
    //                     ] |> Some
    //     else None


    //// aib_plant(ref, name, type, parent).
    //let makePlantFact (namePattern : string) (row : FlocRow) : Predicate option = 
    //    if row.Category = "PLANT" 
    //           && Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
    //        predicate "aib_plant" 
    //                    [ quotedAtom row.Reference
    //                    ; quotedAtom row.AssetName
    //                    ; quotedAtom row.AssetType
    //                    ; quotedAtom row.ParentRef
    //                    ] |> Some
    //    else None

    //// aib_plant_item(ref, name, type, parent).
    //let makePlantItemFact (namePattern : string) (row : FlocRow) : Predicate option = 
    //    if row.Category = "PLANT ITEM"
    //           && Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
    //        predicate "aib_plant_item" 
    //                    [ quotedAtom row.Reference
    //                    ; quotedAtom row.AssetName
    //                    ; quotedAtom row.AssetType
    //                    ; quotedAtom row.ParentRef
    //                    ] |> Some
    //    else None

    //// aib_installation(ref, name, type, parent).
    //let makeEquipmentFact (namePattern : string) (row : EquipmentRow) : Predicate option = 
    //    if Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
    //        predicate "aib_equipment" 
    //                    [ quotedAtom row.Reference
    //                    ; quotedAtom row.AssetName
    //                    ; quotedAtom row.AssetType
    //                    ; quotedAtom row.ParentRef
    //                    ] |> Some
    //    else None


    //let generateExportFacts (commonNameFilterPattern : string) 
    //                           (moduleName : string)
    //                           (flocRows : FlocRow list) 
    //                           (equipRows : EquipmentRow list) 
    //                           (outputFile : string) : unit =             
    //    let proc =  
    //        factWriter { 
    //            do! generatePredicates (makeInstallationFact commonNameFilterPattern) flocRows
    //            do! generatePredicates (makeProcessGroupFact commonNameFilterPattern) flocRows
    //            do! generatePredicates (makeProcessFact commonNameFilterPattern) flocRows
    //            do! generatePredicates (makePlantFact commonNameFilterPattern) flocRows
    //            do! generatePredicates (makePlantItemFact commonNameFilterPattern) flocRows
    //            do! generatePredicates (makeEquipmentFact commonNameFilterPattern) equipRows
    //            return ()
    //        }
    //    let exportlist = 
    //        [ "aib_installation/3"
    //        ; "aib_process_group/4"
    //        ; "aib_process/4"
    //        ; "aib_plant/4"
    //        ; "aib_plant_item/4"
    //        ; "aib_equipment/4" ]
    //    writeFactsWithModuleDecl outputFile moduleName exportlist proc

