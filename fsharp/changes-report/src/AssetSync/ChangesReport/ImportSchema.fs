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


    