// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module ImportSchema =
    
    open FSharp.Data

    // ************************************************************************
    // Table : asset_change

    [<Literal>]
    let ChangeReqAssetsSchema = 
        "AideAssetId(int64),\
        ChangeRequestId(int64),\
        ChangeRequestTime(date),\
        ChangeRequestType(string),\
        ChangeRequestStatus(string),\
        ChangeRequestComments(string),\
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
        

    //[<Literal>]
    //let ChangeReqAssetsSample =
    //    "20000,10000,\
    //    2019-05-17T11:57:18,\
    //    Change Request Type,Change Request Status,\
    //    Change Request Comments,\
    //    Ai Asset Reference,Aide Asset Reference,\
    //    Ai Asset Name,Ai Common Name,\
    //    2019-05-17T11:57:18,Ai Manufacturer,\
    //    Ai Model,Ai Hierarchy Key,\
    //    Ai Asset Status,Ai Location Reference,\
    //    0,\
    //    Aide Asset Reference,Aide Asset Reference,\
    //    Aide Asset Name,Aide Common Name,\
    //    2019-05-17T11:57:18,Aide Manufacturer,\
    //    Aide Model, Aide Hierarchy Key,\
    //    Aide Asset Status,Aide Location Reference,\
    //    1"
    
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
        ChangeRequestTime(date),\
        ChangeRequestType(string),\
        ChangeRequestStatus(string),\
        ChangeRequestComments(string),\
        AssetReference(string),\
        AssetCommonName(string),\
        AttributeName(string),\
        AiValue(string option),\
        AiLookupValue(string option),\
        AideValue(string option),\
        AideLookupValue(string option)"


    //[<Literal>]
    //let ChangeReqAttributesSample = 
    //    "100000000,10000,\
    //    2019-05-17T11:57:18, Change Request Type,\
    //    Change Request Status,Change Request Comments,\
    //    Asset Reference,Asset Common Name,\
    //    Attribute Name,\
    //    Ai Value, Ai Lookup Value,\
    //    Aide Value, Aide Lookup Value"
        

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
         ChangeRequestTime(date),\
         ChangeRequestType(string),\
         ChangeRequestStatus(string),\
         ChangeRequestComments(string),\
         AssetReference(string),\
         AssetCommonName(string),\
         AttributeName(string),\
         AttributeSetName(string option),\
         AiValue(string option),\
         AiLookupValue(string option),\
         AideValue(string option),\
         AideLookupValue(string option)"

    //[<Literal>]
    //let RepeatedAttributeChangeSample = 
    //    "100000000,10000,Submitted,ABC01,ASSET_SHORT_NAME,\
    //    ASSET_COMMON_NAME,Attribute Name, Attribute Set Name,\
    //    VALUE1,100,LOOKUP1,\
    //    VALUE2,101,LOOKUP2,2019-05-17T11:57:18.283"
        

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


    