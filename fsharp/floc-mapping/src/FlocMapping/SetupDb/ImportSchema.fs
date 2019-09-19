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

    // ************************************************************************
    // S4 Floc

    // This table is too complicated to provide a schema
    // Edit the path here as necessary
    type S4FlocTable = 
        CsvProvider< @"G:\work\Projects\asset_sync\floc_mapping\S4_Floc_Mapping_Site-A-Z_General_Structure_Initial.csv"
                   , MissingValues = @"#N/A,NULL"
                   , CacheRows = false
                   , PreferOptionals = true
                   , AssumeMissingValues = true
                   >
    
    type S4FlocRow = S4FlocTable.Row


    let readS4FlocTable () : Result<S4FlocTable, string> =
        try 
            let table = new S4FlocTable () in Ok table
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // S4 Equipment


    // This table is too complicated to provide a schema
    // Edit the path here as necessary
    type S4EquipmentTable = 
        CsvProvider< @"G:\work\Projects\asset_sync\floc_mapping\equipment_migration_s1.csv"
                   , PreferOptionals = true >
    
    type S4EquipmentRow = S4EquipmentTable.Row

    let readS4EquipmentTable () : Result<S4EquipmentTable, string> =
        try 
            let table = new S4EquipmentTable () in Ok table
        with
        | ex -> Error ex.Message