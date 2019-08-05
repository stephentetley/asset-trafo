// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.PrologFacts


module SwiAibDbExport =

    open FSharp.Data
    open FactX
    open FactX.FactWriter
    
    
    open AssetTrafo.Base.FactsCommon
    open AssetTrafo.PrologFacts.AibDbExportCommon




    
    let makeEquipmentFact (row : EquipmentRow) : Predicate option = 
        predicate "aib_equipment" 
                    [ quotedAtom row.Reference
                    ; quotedAtom row.AssetType
                    ; quotedAtom row.AssetName
                    ; quotedAtom row.CommonName
                    ; quotedAtom row.ParentRef
                    ] |> Some

    let generateEquipmentFacts (moduleName : string)
                               (rows : EquipmentRow list) 
                               (outputFile : string) : unit =            
        let proc = generatePredicates makeEquipmentFact rows
        let exportlist = 
            [ "aib_equipment/5" ]
        writeFactsWithModuleDecl outputFile moduleName exportlist proc

