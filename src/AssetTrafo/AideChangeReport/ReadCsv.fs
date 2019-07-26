﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AideChangeReport


module ReadCsv =
    
    open FSharp.Data

    open AssetTrafo.AideChangeReport.Syntax

    [<Literal>]
    let AttributeChangeSchema = 
        "ChangeRequestId(int64),RequestStatus(string),\
         Reference(string),AssetName(string),\
         AttributeName(string),AiValue(string option),\
         AiLookupValue(string option),AiLookupCode(int64 option),\
         AideValue(string option),AideLookupValue(string option),\
         AideLookupCode(int64 option),ChangeRequestTime(date)"


    [<Literal>]
    let AttributeChangeSample = 
        "10000,Submitted,ABC01,SHORT_NAME,ATTR_NAME,\
        VALUE1,100,LOOKUP1,\
        VALUE2,101,LOOKUP2,2019-05-17T11:57:18.283"
        

    type AttributeChangeExport = 
        CsvProvider< Schema = AttributeChangeSchema
                   , Sample = AttributeChangeSample
                   , HasHeaders = true >

    type AttributeChangeRow = AttributeChangeExport.Row
    
    let readAttributeChangeExport(path:string) : Result<seq<AttributeChangeRow>, string> = 
        try 
            let table = AttributeChangeExport.Load(uri = path)
            table.Rows |> Ok
        with
        | ex -> Error ex.Message

    let getValueSource (valueCode : int64 option) : ValueSource = 
        match valueCode with
        | None -> Freetext
        | _ -> Lookup

    let getValue (value : string option) 
                    (lookupCode : int64 option) 
                    (lookupValue : string option) : string = 
        match lookupCode with
        | None -> Option.defaultValue "" value
        | Some _ -> Option.defaultValue "" lookupValue

    let convertAttributeChangeRow (row : AttributeChangeRow) : AttributeChange = 
        { ChangeRequestId = row.ChangeRequestId
          AttributeName = row.AttributeName
          AiValue = getValue row.AiValue row.AiLookupCode row.AiLookupValue
          AiSource = getValueSource row.AideLookupCode
          AideValue = getValue row.AideValue row.AideLookupCode row.AideLookupValue
          AideSource = getValueSource row.AiLookupCode
        }

    // ************************************************************************

    [<Literal>]
    let AssetChangeSchema = 
        "ChangeRequestId(int64),RequestStatus(string),\
         ChangeRequestType(string),AssetReference(string),\
         AiAssetName(string),AiCommonName(string),\
         AiInstalledFromDate(date),AiManufacturer(string),\
         AiModel(string),AiHierarchyKey(string),\
         AiAssetStatus(string),AiLocationReference(string),\
         AideAssetName(string),AideCommonName(string),\
         AideInstalledFromDate(date),AideManufacturer(string),\
         AideModel(string),AideHierarchyKey(string),\
         AideAssetStatus(string),AideLocationReference(string),\
         ChangeRequestTime(date)"        

    [<Literal>]
    let AssetChangeSample =
        "10000,Submitted,Attribute,CODE000,\
         SHORT_NAME,LONG_NAME,1997-01-01T00:00:00,\
         M1,MM1,XYZ,OPERATIONAL,SE7826004748,\
         SHORT_NAME,LONG_NAME,1997-01-01T00:00:00,\
         M1,MM1,XYZ,OPERATIONAL,SE7826004748,\
         2019-05-17T11:57:18.283"
    
    type AssetChangeExport = 
        CsvProvider< Schema = AssetChangeSchema                              
                   , Sample = AssetChangeSample
                   , HasHeaders = true >

    type AssetChangeRow = AssetChangeExport.Row
    
    let readAssetChangeExport(path:string) : Result<seq<AssetChangeRow>, string> = 
        try 
            let table = AssetChangeExport.Load(uri = path)
            table.Rows |> Ok
        with
        | ex -> Error ex.Message

    let readProperties (row : AssetChangeRow) : AssetProperty list = 
        [ { PropertyName    = "Name"
            ; AiValue       = row.AiAssetName 
            ; AideValue     = row.AideAssetName }
        ; { PropertyName    = "Common Name"
            ; AiValue       = row.AiCommonName
            ; AideValue     = row.AideCommonName }
        ; { PropertyName    = "Installed From Date"
            ; AiValue       = row.AiInstalledFromDate.ToString(format="dd/MM/yyyy hh:mm:ss")
            ; AideValue     = row.AideInstalledFromDate.ToString(format="dd/MM/yyyy hh:mm:ss") }
        ; { PropertyName    = "Manufacturer"
            ; AiValue       = row.AiManufacturer
            ; AideValue     = row.AideManufacturer }
        ; { PropertyName    = "Model"
            ; AiValue       = row.AiModel
            ; AideValue     = row.AideModel }
        ; { PropertyName    = "Hierarchy Key"
            ; AiValue       = row.AiHierarchyKey
            ; AideValue     = row.AideHierarchyKey }
        ; { PropertyName    = "Asset Status"
            ; AiValue       = row.AiAssetStatus
            ; AideValue     = row.AideAssetStatus }
        ; { PropertyName    = "Location Ref"
            ; AiValue       = row.AiLocationReference
            ; AideValue     = row.AideLocationReference }
        ]
    let convertAssetChangeRow (row : AssetChangeRow) : AssetChange = 
        { ChangeRequestId = row.ChangeRequestId
          Reference = row.AssetReference
          AiAssetName = row.AiAssetName
          AiCommonName = row.AiCommonName
          AssetProperties = readProperties row
        }