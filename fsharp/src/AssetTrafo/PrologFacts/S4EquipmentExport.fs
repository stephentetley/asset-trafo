// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.PrologFacts


module S4EquipmentExport =

    open System.Text.RegularExpressions
    open FSharp.Data

    open FactX
    open FactX.FactWriter

    open AssetTrafo.Base.FactsCommon

    // ********** DATA SETUP **********
    type S4EquipmentTable = 
        CsvProvider<Sample = @"G:\work\Projects\asset_sync\equipment_migration_s1.csv"
                    , PreferOptionals = true >
    
    type S4EquipmentRow = S4EquipmentTable.Row


    
    let getEquipmentRows(cvsPath : string) : S4EquipmentRow list = 
        let table = S4EquipmentTable.Load(uri = cvsPath) in Seq.toList table.Rows


    let checkString (input : string) : string option = 
        match input with
        | null | "NULL" -> None
        | _ -> input.Trim() |> Some

    let checkedAtom (input : string option) : Term = 
        match input with
        | None -> nullTerm
        | Some str -> 
            match str with
            | null | "NULL" -> nullTerm
            | _ -> str.Trim() |> quotedAtom 



    // ************************************************************************
    // Description Lookups (code to description) (S4)

    let s4EquipmentFact (filterPattern : string)  (row : S4EquipmentRow) : Predicate option = 
        if Regex.IsMatch(input = row.CommonName, pattern = filterPattern) then 
            match (row.``Migration Status (Y/N)`` ,
                    row.``400 S/4 Equip Reference``,
                    checkString row.``AI2 AIB Reference``) with
            | true, Some s4Ref, Some aibRef -> 
                predicate "s4_equipment" 
                            [ intTerm s4Ref
                            ; quotedAtom aibRef
                            ; checkedAtom row.``Equipment Description``
                            ; quotedAtom row.Category
                            ; checkedAtom row.``Object Type``
                            ; checkedAtom row.Class
                            ; quotedAtom row.``L6_Floc Code``
                            ] |> Some
            | _, _, _-> None
        else None


    let swiS4Equipment (filterPattern : string) 
                       (moduleName : string)
                       (equipmentRows : S4EquipmentRow list)
                       (outputFile : string) : unit =  
        let proc = 
            factWriter { 
                do! generatePredicates (s4EquipmentFact filterPattern) equipmentRows
                return ()
            }
        let exportlist = [ "s4_equipment/7"]
        writeFactsWithModuleDecl outputFile moduleName exportlist proc
               



