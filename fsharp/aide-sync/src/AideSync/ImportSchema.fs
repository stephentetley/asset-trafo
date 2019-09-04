// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module ImportSchema =
    
    open FSharp.Data


    // ************************************************************************
    // Table : change_request

    [<Literal>]
    let ChangeRequestSchema = 
        "ChangeRequestId(int64),\
        ChangeRequestTime(date),\
        ChangeRequestType(string),\
        ChangeRequestStatus(string),\
        ChangeRequestComments(string)"
        


    
    type ChangeRequestTable = 
        CsvProvider< Schema = ChangeRequestSchema                              
                   , Sample = ChangeRequestSchema
                   , HasHeaders = true >

    type ChangeRequestRow = ChangeRequestTable.Row
    
    let readChangeRequestExport(path:string) : Result<ChangeRequestTable, string> = 
        try 
            ChangeRequestTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message

    
    
    // ************************************************************************
    // ai_asset table

    [<Literal>]
    let AiAssetSchema = 
         "AssetId(int64),\
         Reference(string),\
         AssetCommonName(string),\
         AssetName(string),\
         AssetType(string),\
         AssetCategory(string)"    



    type AiAssetTable = 
        CsvProvider< Schema = AiAssetSchema 
                   , Sample = AiAssetSchema
                   , HasHeaders = true >

    type AiAssetRow = AiAssetTable.Row

    let readAiAssetExport(path:string) : Result<AiAssetTable, string> = 
        try 
            AiAssetTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message

    // ************************************************************************
    // aide_asset table

    [<Literal>]
    let AideAssetSchema = 
        "AideAssetId(int64),\
         ChangeRequestId(int64 option),\
         AssetId(int64 option),\
         Reference(string),\
         AssetCommonName(string),\
         AssetName(string),\
         AssetType(string),\
         AssetCategory(string)"    



    type AideAssetTable = 
        CsvProvider< Schema = AideAssetSchema 
                   , Sample = AideAssetSchema
                   , HasHeaders = true >

    type AideAssetRow = AideAssetTable.Row

    let readAideAssetExport(path:string) : Result<AideAssetTable, string> = 
        try 
            AideAssetTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // Table : asset_change_change

    [<Literal>]
    let AssetChangeSchema = 
        "AideAssetId(int64),\
        AiAssetId(int64 option),\
        ChangeRequestId(int64),\
        SchemeId(int64 option),\
        AiAssetReference(string),\
        AideAssetReference(string),\
        AiAssetName(string),\
        AiCommonName(string),\
        AiInstalledFromDate(date),\
        AiManufacturer(string option),\
        AiModel(string option),\
        AiHierarchyKey(string),\
        AiAssetStatus(string),\
        AiLocationReference(string),\
        AiAssetDeleted(int),\
        AideAssetName(string),\
        AideCommonName(string),\
        AideInstalledFromDate(date),\
        AideManufacturer(string option),\
        AideModel(string option),\
        AideHierarchyKey(string),\
        AideAssetStatus(string),\
        AideLocationReference(string),\
        AideAssetDeleted(int)"
        

    
    type AssetChangeTable = 
        CsvProvider< Schema = AssetChangeSchema                              
                   , Sample = AssetChangeSchema
                   , HasHeaders = true >

    type AssetChangeRow = AssetChangeTable.Row
    
    let readAssetChangeExport(path:string) : Result<AssetChangeTable, string> = 
        try 
            AssetChangeTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // Table : asset_attribute_change
    
    [<Literal>]
    let AttributeChangeSchema = 
        "AssetAttrValueId(int64),\
        ChangeRequestId(int64),\
        AssetId(int64),\
        AssetReference(string),\
        AssetName(string),\
        AssetCommonName(string),\
        AttributeName(string),\
        AiValue(string option),\
        AiLookupValue(string option),\
        AideValue(string option),\
        AideLookupValue(string option)"


        

    type AttributeChangeTable = 
        CsvProvider< Schema = AttributeChangeSchema
                   , Sample = AttributeChangeSchema
                   , HasHeaders = true >

    type AttributeChangeRow = AttributeChangeTable.Row
    
    let readAttributeChangeExport(path:string) : Result<AttributeChangeTable, string> = 
        try 
            AttributeChangeTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // Table : asset_repeated_attribute_change


    [<Literal>]
    let RepeatedAttributeChangeSchema = 
        "AssetAttrRepeatingValueId(int64),\
         ChangeRequestId(int64),\
         AssetId(int64),\
         AssetReference(string),\
         AssetName(string),\
         AssetCommonName(string),\
         AttributeName(string),\
         AttributeSetName(string option),\
         AiValue(string option),\
         AiLookupValue(string option),\
         AideValue(string option),\
         AideLookupValue(string option)"


        

    type RepeatedAttributeChangeTable = 
        CsvProvider< Schema = RepeatedAttributeChangeSchema
                   , Sample = RepeatedAttributeChangeSchema
                   , HasHeaders = true >

    type RepeatedAttributeChangeRow = RepeatedAttributeChangeTable.Row
    
    let readRepeatedAttributeChangeExport(path:string) : Result<RepeatedAttributeChangeTable, string> = 
        try 
            RepeatedAttributeChangeTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // Table : work_scheme


    [<Literal>]
    let WorkSchemeSchema = 
        "SchemeId(int64),\
         SchemeCode(string),\
         SchemeName(string),\
         Description(string),\
         SolutionProvider(string)"
        

    type WorkSchemeTable = 
        CsvProvider< Schema = WorkSchemeSchema
                   , Sample = WorkSchemeSchema
                   , HasHeaders = true >

    type WorkSchemeRow = WorkSchemeTable.Row
    
    let readWorkSchemeExport(path:string) : Result<WorkSchemeTable, string> = 
        try 
            WorkSchemeTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message



    // ************************************************************************
    // Table : asset : new asset

    [<Literal>]
    let AssetNewSchema = 
        "AideAssetId(int64),\
        AideAssetReference(string),\
        AideAssetName(string),\
        AideCommonName(string),\
        AideInstalledFromDate(date),\
        AideManufacturer(string option),\
        AideModel(string option),\
        AideHierarchyKey(string),\
        AideAssetStatus(string),\
        AideLocationReference(string),\
        AideAssetDeleted(int)"
        

    
    type AssetNewTable = 
        CsvProvider< Schema = AssetNewSchema                              
                   , Sample = AssetNewSchema
                   , HasHeaders = true >

    type AssetNewRow = AssetNewTable.Row
    
    let readAssetNewExport(path:string) : Result<AssetNewTable, string> = 
        try 
            AssetNewTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // Table : asset_attribute : new attribute
    
    [<Literal>]
    let AttributeNewSchema = 
        "AssetAttrValueId(int64),\
        AssetReference(string),\
        AssetName(string),\
        AssetCommonName(string),\
        AttributeName(string),\
        AideValue(string option),\
        AideLookupValue(string option)"


    type AttributeNewTable = 
        CsvProvider< Schema = AttributeNewSchema
                   , Sample = AttributeNewSchema
                   , HasHeaders = true >

    type AttributeNewRow = AttributeNewTable.Row
    
    let readAttributeNewExport(path:string) : Result<AttributeNewTable, string> = 
        try 
            AttributeNewTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message



    // ************************************************************************
    // Table: aide_structure_relationships

    [<Literal>]
    let AideStructureRelationshipSchema = 
        "StructureRelationshipId(int64),\
         ParentAideAssetId(int64),\
         ChildAideAssetId(int64)"    



    type AideStructureRelationshipTable = 
        CsvProvider< Schema = AideStructureRelationshipSchema 
                   , Sample = AideStructureRelationshipSchema
                   , HasHeaders = true >

    type AideStructureRelationshipRow = AideStructureRelationshipTable.Row

    let getAideStructureRelationshipRows (cvsPath : string) : Result<seq<AideStructureRelationshipRow>, string> = 
        try
            let table = AideStructureRelationshipTable.Load(uri = cvsPath)
            table.Rows |> Ok
        with
        | excn -> Error excn.Message


    // ************************************************************************
    // Table: ai_structure_relationships

    [<Literal>]
    let AiStructureRelationshipSchema = 
        "StructureRelationshipId(int64),\
         ParentAssetId(int64),\
         ChildAssetId(int64)"    



    type AiStructureRelationshipTable = 
        CsvProvider< Schema = AiStructureRelationshipSchema 
                   , Sample = AiStructureRelationshipSchema
                   , HasHeaders = true >

    type AiStructureRelationshipRow = AiStructureRelationshipTable.Row

    let getAiStructureRelationshipRows (cvsPath : string) : Result<seq<AiStructureRelationshipRow>, string> = 
        try
            let table = AiStructureRelationshipTable.Load(uri = cvsPath)
            table.Rows |> Ok
        with
        | excn -> Error excn.Message

