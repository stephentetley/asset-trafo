// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport.Old


module ImportSchema =
    
    open FSharp.Data

    // ************************************************************************
    // Table : asset_change

    [<Literal>]
    let AssetChangeSchema = 
        "ChangeRequestId(int64),RequestStatus(string),\
         ChangeRequestType(string),AssetReference(string),\
         AiAssetName(string),AiCommonName(string),\
         AiInstalledFromDate(date),AiManufacturer(string),\
         AiModel(string),AiHierarchyKey(string),\
         AiAssetStatus(string),AiLocationReference(string),\
         AiAssetDeleted(bool),\
         AideAssetName(string),AideCommonName(string),\
         AideInstalledFromDate(date),AideManufacturer(string),\
         AideModel(string),AideHierarchyKey(string),\
         AideAssetStatus(string),AideLocationReference(string),\
         AideAssetDeleted(bool),\
         ChangeRequestTime(date)"        

    [<Literal>]
    let AssetChangeSample =
        "10000,Submitted,Attribute,CODE000,\
         SHORT_NAME,LONG_NAME,1997-01-01T00:00:00,\
         M1,MM1,XYZ,OPERATIONAL,SE7826004748,\
         0,\
         SHORT_NAME,LONG_NAME,1997-01-01T00:00:00,\
         M1,MM1,XYZ,OPERATIONAL,SE7826004748,\
         0,\
         2019-05-17T11:57:18.283"
    
    type AssetChangeTable = 
        CsvProvider< Schema = AssetChangeSchema                              
                   , Sample = AssetChangeSample
                   , HasHeaders = true >

    type AssetChangeRow = AssetChangeTable.Row
    
    let readAssetChangeExport(path:string) : Result<AssetChangeTable, string> = 
        try 
            AssetChangeTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // Table : attribute_change

    [<Literal>]
    let AttributeChangeSchema = 
        "AideAssetAttributeValueId(int64),\
         ChangeRequestId(int64),RequestStatus(string),\
         Reference(string),AiAssetName(string),\
         AiAssetCommonName(string),\
         AttributeName(string),AiValue(string option),\
         AiLookupValue(string option),AiLookupCode(int64 option),\
         AideValue(string option),AideLookupValue(string option),\
         AideLookupCode(int64 option),ChangeRequestTime(date)"


    [<Literal>]
    let AttributeChangeSample = 
        "100000000,10000,Submitted,ABC01,ASSET_SHORT_NAME,\
        ASSET_COMMON_NAME,ATTR_NAME,\
        VALUE1,100,LOOKUP1,\
        VALUE2,101,LOOKUP2,2019-05-17T11:57:18.283"
        

    type AttributeChangeTable = 
        CsvProvider< Schema = AttributeChangeSchema
                   , Sample = AttributeChangeSample
                   , HasHeaders = true >

    type AttributeChangeRow = AttributeChangeTable.Row
    
    let readAttributeChangeExport(path:string) : Result<AttributeChangeTable, string> = 
        try 
            AttributeChangeTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    // ************************************************************************
    // Table : repeated_attribute_change


    [<Literal>]
    let RepeatedAttributeChangeSchema = 
        "AideAssetAttributeValueId(int64),\
         ChangeRequestId(int64),RequestStatus(string),\
         Reference(string),AiAssetName(string),\
         AiAssetCommonName(string),\
         AttributeName(string),AttributeSetName(string),\
         AiValue(string option),\
         AiLookupValue(string option),AiLookupCode(int64 option),\
         AideValue(string option),AideLookupValue(string option),\
         AideLookupCode(int64 option),ChangeRequestTime(date)"


    [<Literal>]
    let RepeatedAttributeChangeSample = 
        "100000000,10000,Submitted,ABC01,ASSET_SHORT_NAME,\
        ASSET_COMMON_NAME,Attribute Name, Attribute Set Name,\
        VALUE1,100,LOOKUP1,\
        VALUE2,101,LOOKUP2,2019-05-17T11:57:18.283"
        

    type RepeatedAttributeChangeTable = 
        CsvProvider< Schema = RepeatedAttributeChangeSchema
                   , Sample = RepeatedAttributeChangeSample
                   , HasHeaders = true >

    type RepeatedAttributeChangeRow = RepeatedAttributeChangeTable.Row
    
    let readRepeatedAttributeChangeExport(path:string) : Result<RepeatedAttributeChangeTable, string> = 
        try 
            RepeatedAttributeChangeTable.Load(uri = path) |> Ok
        with
        | ex -> Error ex.Message


    