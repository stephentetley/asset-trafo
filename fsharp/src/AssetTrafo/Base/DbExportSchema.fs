// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Base


module DbExportSchema =

    open FSharp.Data


    // ************************************************************************
    // Floc


    [<Literal>]
    let AibFlocTableSchema = 
        "Reference(string),HKey(string),\
         AssetName(string),AssetType(string),\
         AssetCode(string), Category(string),\
         CommonName(string),ParentRef(string)"    

    [<Literal>]
    let AibFlocTableSample = 
         "SAI0101,2OLDWW,NO 1 STARTER,MOTOR STARTER,MOTR,PLANT ITEM,COMMON NAME/WITH/SEPARATORS,SAI0100"
     

    type AibFlocTable = 
        CsvProvider< Schema = AibFlocTableSchema
                   , Sample = AibFlocTableSample
                   , HasHeaders = true >

    type AibFlocRow = AibFlocTable.Row

    let getAibFlocRows (cvsPath : string) : AibFlocRow list = 
        let table = AibFlocTable.Load(uri = cvsPath)
        table.Rows |> Seq.toList

    // ************************************************************************
    // Equipment


    [<Literal>]
    let AibEquipmentTableSchema = 
        "Reference(string),\
         AssetName(string),AssetType(string),\
         Category(string),CommonName(string),\
         ParentRef(string)"    

    [<Literal>]
    let AibEquipmentTableSample = 
         "SAI0101,EQUIPMENT: NO 1 STARTER,EQUIPMENT: NO 1 STARTER,MECHANICAL,COMMON NAME/WITH/SEPARATORS,SAI0100"
     

    type AibEquipmentTable = 
        CsvProvider< Schema = AibEquipmentTableSchema
                   , Sample = AibEquipmentTableSample
                   , HasHeaders = true >

    type AibEquipmentRow = AibEquipmentTable.Row

    let getAibEquipmentRows (cvsPath : string) : AibEquipmentRow list = 
        let table = AibEquipmentTable.Load(uri = cvsPath)
        table.Rows |> Seq.toList


