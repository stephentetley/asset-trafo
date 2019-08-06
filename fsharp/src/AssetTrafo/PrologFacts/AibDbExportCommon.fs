// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.PrologFacts


module AibDbExportCommon =

    open FSharp.Data


    // ************************************************************************
    // Floc


    [<Literal>]
    let FlocTableSchema = 
        "Reference(string),HKey(string),\
         AssetName(string),AssetType(string),\
         AssetCode(string), Category(string),\
         CommonName(string),ParentRef(string)"    

    [<Literal>]
    let FlocTableSample = 
         "SAI0101,2OLDWW,NO 1 STARTER,MOTOR STARTER,MOTR,PLANT ITEM,COMMON NAME/WITH/SEPARATORS,SAI0100"
     

    type FlocTable = 
        CsvProvider< Schema = FlocTableSchema
                   , Sample = FlocTableSample
                   , HasHeaders = true >

    type FlocRow = FlocTable.Row

    let getFlocRows (cvsPath : string) : FlocRow list = 
        let table = FlocTable.Load(uri = cvsPath)
        table.Rows |> Seq.toList

    // ************************************************************************
    // Equipment


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


