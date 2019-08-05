// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.PrologFacts


module AibEquipmentFacts =

    open FSharp.Data
    open FactX
    open FactX.FactWriter
    
    
    open AssetTrafo.Base.FactsCommon

    // ********** DATA SETUP **********

    [<Literal>]
    let EquipmentTableSchema = 
        "Reference(string),\
         AssetName(string),AssetType(string),\
         Category(string),CommonName(string),\
         ParentRef(string)"    

    [<Literal>]
    let EquipmentTableSample = 
         "SAI0101,EQUIPMENT: NO 1 STARTER,EQUIPMENT: NO 1 STARTER,MECHANICAL,COMMON NAME/WITH/SEPARATORS,SAI0100"
     

    type EquipmentTable = 
        CsvProvider< Schema = EquipmentTableSchema
                   , Sample = EquipmentTableSample
                   , HasHeaders = true >

    type EquipmentRow = EquipmentTable.Row

    let getEquipmentRows (cvsPath : string) : EquipmentRow list = 
        let table = EquipmentTable.Load(uri = cvsPath)
        table.Rows |> Seq.toList



    
    let makeEquipmentFact (row : EquipmentRow) : Predicate option = 
        predicate "aib_equipment" 
                    [ quotedAtom row.Reference
                    ; quotedAtom row.AssetType
                    ; quotedAtom row.AssetName
                    ; quotedAtom row.CommonName
                    ; quotedAtom row.ParentRef
                    ] |> Some

    let generateEquipmentFacts (rows : EquipmentRow list) 
                                (outputFile : string) : unit =            
            writeFactsWithHeaderComment outputFile
                <| generatePredicates makeEquipmentFact rows


