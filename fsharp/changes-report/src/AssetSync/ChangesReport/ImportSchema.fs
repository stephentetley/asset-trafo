// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module ImportSchema =
    
    open FSharp.Data
    // ************************************************************************
    // Table : asset_change

    [<Literal>]
    let ChangeRequestsSchema = 
        "ChangeRequestId(int64),\
        ChangeRequestTime(date),\
        ChangeRequestType(string),\
        ChangeRequestStatus(string),\
        ChangeRequestComments(string)"
        


    
    type ChangeRequestsTable = 
        CsvProvider< Schema = ChangeRequestsSchema                              
                   , Sample = ChangeRequestsSchema
                   , HasHeaders = true >

    type ChangeRequestsRow = ChangeRequestsTable.Row
    
    let readChangeRequestsExport(path:string) : Result<ChangeRequestsTable, string> = 
        try 
            ChangeRequestsTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // Table : asset_change

    [<Literal>]
    let ChangeReqAssetsSchema = 
        "AideAssetId(int64),\
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
        

    
    type ChangeReqAssetsTable = 
        CsvProvider< Schema = ChangeReqAssetsSchema                              
                   , Sample = ChangeReqAssetsSchema
                   , HasHeaders = true >

    type ChangeReqAssetsRow = ChangeReqAssetsTable.Row
    
    let readChangeReqAssetsExport(path:string) : Result<ChangeReqAssetsTable, string> = 
        try 
            ChangeReqAssetsTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // Table : attribute_change
    
    [<Literal>]
    let ChangeReqAttributesSchema = 
        "AssetAttrValueId(int64),\
        ChangeRequestId(int64),\
        AssetReference(string),\
        AssetName(string),\
        AssetCommonName(string),\
        AttributeName(string),\
        AiValue(string option),\
        AiLookupValue(string option),\
        AideValue(string option),\
        AideLookupValue(string option)"


        

    type ChangeReqAttributesTable = 
        CsvProvider< Schema = ChangeReqAttributesSchema
                   , Sample = ChangeReqAttributesSchema
                   , HasHeaders = true >

    type ChangeReqAttributesRow = ChangeReqAttributesTable.Row
    
    let readChangeReqAttributesExport(path:string) : Result<ChangeReqAttributesTable, string> = 
        try 
            ChangeReqAttributesTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // Table : repeated_attribute_change


    [<Literal>]
    let ChangeReqRepeatedAttributesSchema = 
        "AssetAttrRepeatingValueId(int64),\
         ChangeRequestId(int64),\
         AssetReference(string),\
         AssetName(string),\
         AssetCommonName(string),\
         AttributeName(string),\
         AttributeSetName(string option),\
         AiValue(string option),\
         AiLookupValue(string option),\
         AideValue(string option),\
         AideLookupValue(string option)"


        

    type ChangeReqRepeatedAttributesTable = 
        CsvProvider< Schema = ChangeReqRepeatedAttributesSchema
                   , Sample = ChangeReqRepeatedAttributesSchema
                   , HasHeaders = true >

    type ChangeReqRepeatedAttributesRow = ChangeReqRepeatedAttributesTable.Row
    
    let readChangeReqRepeatedAttributesExport(path:string) : Result<ChangeReqRepeatedAttributesTable, string> = 
        try 
            ChangeReqRepeatedAttributesTable.Load(uri = path) |> Ok
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
    // Table : asset_change : new asset

    [<Literal>]
    let NoChangeReqNewAssetsSchema = 
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
        

    
    type NoChangeReqNewAssetsTable = 
        CsvProvider< Schema = NoChangeReqNewAssetsSchema                              
                   , Sample = NoChangeReqNewAssetsSchema
                   , HasHeaders = true >

    type NoChangeReqNewAssetsRow = NoChangeReqNewAssetsTable.Row
    
    let readNoChangeReqNewAssetsExport(path:string) : Result<NoChangeReqNewAssetsTable, string> = 
        try 
            NoChangeReqNewAssetsTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // Table : attribute_change : new attribute
    
    [<Literal>]
    let NoChangeReqNewAttributesSchema = 
        "AssetAttrValueId(int64),\
        AssetReference(string),\
        AssetName(string),\
        AssetCommonName(string),\
        AttributeName(string),\
        AideValue(string option),\
        AideLookupValue(string option)"


    type NoChangeReqNewAttributesTable = 
        CsvProvider< Schema = NoChangeReqNewAttributesSchema
                   , Sample = NoChangeReqNewAttributesSchema
                   , HasHeaders = true >

    type NoChangeReqNewAttributesRow = NoChangeReqNewAttributesTable.Row
    
    let readNoChangeReqNewAttributesExport(path:string) : Result<NoChangeReqNewAttributesTable, string> = 
        try 
            NoChangeReqNewAttributesTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message
