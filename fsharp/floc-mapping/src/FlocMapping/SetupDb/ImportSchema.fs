// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.SetupDb

module ImportSchema = 
    
    open FSharp.Data

    // ************************************************************************
    // Aib floc

    [<Literal>]
    let AibFlocSchema = 
        "Reference(string),\
        HKey(string),\
        AssetName(string),\
        AssetType(string),\
        AssetCode(string),\
        Category(string),\
        CommonName(string),\
        ParentRef(string)"

    type AibFlocTable = 
        CsvProvider< Schema = AibFlocSchema                              
                   , Sample = AibFlocSchema
                   , HasHeaders = true >

    type AibFlocRow = AibFlocTable.Row
    
    let readAibFlocExport(path:string) : Result<AibFlocTable, string> = 
        try 
            AibFlocTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message

    // ************************************************************************
    // Aib equipment

    [<Literal>]
    let AibEquipmentSchema = 
        "Reference(string),\
        AssetName(string),\
        AssetType(string),\
        Category(string),\
        CommonName(string),\
        ParentRef(string)"

    type AibEquipmentTable = 
        CsvProvider< Schema = AibEquipmentSchema                              
                   , Sample = AibEquipmentSchema
                   , HasHeaders = true >

    type AibEquipmentRow = AibEquipmentTable.Row
    
    let readAibEquipmentExport(path:string) : Result<AibEquipmentTable, string> = 
        try 
            AibEquipmentTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message