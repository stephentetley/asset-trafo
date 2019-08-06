// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.PrologFacts


module SwiAibDbExport =

    open System.Text.RegularExpressions

    open FSharp.Data
    open FactX
    open FactX.FactWriter
    
    
    open AssetTrafo.Base.FactsCommon
    open AssetTrafo.PrologFacts.AibDbExportCommon

    // aib_installation(ref, name, type).
    let makeInstallationFact (namePattern : string) (row : FlocRow) : Predicate option = 
         if row.Category = "INSTALLATION" 
                && Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
             predicate "aib_installation" 
                         [ quotedAtom row.Reference
                         ; quotedAtom row.AssetName
                         ; quotedAtom row.AssetCode
                         ] |> Some
         else None

    // aib_process_group(ref, name, type, parent).
    let makeProcessGroupFact (namePattern : string) (row : FlocRow) : Predicate option = 
         if row.Category = "PROCESS GROUP" 
                && Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
             predicate "aib_process_group" 
                         [ quotedAtom row.Reference
                         ; quotedAtom row.AssetName
                         ; quotedAtom row.AssetType
                         ; quotedAtom row.ParentRef
                         ] |> Some
         else None

    // aib_process(ref, name, type, parent).
    let makeProcessFact (namePattern : string) (row : FlocRow) : Predicate option = 
         if row.Category = "PROCESS" 
                && Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
             predicate "aib_process" 
                         [ quotedAtom row.Reference
                         ; quotedAtom row.AssetName
                         ; quotedAtom row.AssetType
                         ; quotedAtom row.ParentRef
                         ] |> Some
         else None


    // aib_plant(ref, name, type, parent).
    let makePlantFact (namePattern : string) (row : FlocRow) : Predicate option = 
        if row.Category = "PLANT" 
               && Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
            predicate "aib_plant" 
                        [ quotedAtom row.Reference
                        ; quotedAtom row.AssetName
                        ; quotedAtom row.AssetType
                        ; quotedAtom row.ParentRef
                        ] |> Some
        else None

    // aib_plant_item(ref, name, type, parent).
    let makePlantItemFact (namePattern : string) (row : FlocRow) : Predicate option = 
        if row.Category = "PLANT ITEM"
               && Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
            predicate "aib_plant_item" 
                        [ quotedAtom row.Reference
                        ; quotedAtom row.AssetName
                        ; quotedAtom row.AssetType
                        ; quotedAtom row.ParentRef
                        ] |> Some
        else None

    // aib_installation(ref, name, type, parent).
    let makeEquipmentFact (namePattern : string) (row : EquipmentRow) : Predicate option = 
        if Regex.IsMatch(input = row.CommonName, pattern = namePattern) then 
            predicate "aib_equipment" 
                        [ quotedAtom row.Reference
                        ; quotedAtom row.AssetName
                        ; quotedAtom row.AssetType
                        ; quotedAtom row.ParentRef
                        ] |> Some
        else None


    let generateExportFacts (commonNameFilterPattern : string) 
                               (moduleName : string)
                               (flocRows : FlocRow list) 
                               (equipRows : EquipmentRow list) 
                               (outputFile : string) : unit =             
        let proc =  
            factWriter { 
                do! generatePredicates (makeInstallationFact commonNameFilterPattern) flocRows
                do! generatePredicates (makeProcessGroupFact commonNameFilterPattern) flocRows
                do! generatePredicates (makeProcessFact commonNameFilterPattern) flocRows
                do! generatePredicates (makePlantFact commonNameFilterPattern) flocRows
                do! generatePredicates (makePlantItemFact commonNameFilterPattern) flocRows
                do! generatePredicates (makeEquipmentFact commonNameFilterPattern) equipRows
                return ()
            }
        let exportlist = 
            [ "aib_installation/3"
            ; "aib_process_group/4"
            ; "aib_process/4"
            ; "aib_plant/4"
            ; "aib_plant_item/4"
            ; "aib_equipment/4" ]
        writeFactsWithModuleDecl outputFile moduleName exportlist proc

