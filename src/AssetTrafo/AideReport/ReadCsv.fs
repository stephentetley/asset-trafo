﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AideReport


module ReadCsv =
    
    open FSharp.Data

    open AssetTrafo.AideReport.Syntax

    [<Literal>]
    let AttributeChangeSchema = 
        "ChangeRequestId(int64),RequestStatus(string),\
         Reference(string),AssetName(string),\
         AttributeName(string),AI2Value(string option),\
         AILookupValue(string option),AILookupCode(int64 option),\
         AIDEValue(string option),AIDELookupValue(string option),\
         AIDELookupCode(int64 option),ChangeRequestTime(date)"


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

    let convertAttributeChangeRow (row : AttributeChangeRow) : AttributeChange = 
        { ChangeRequestId = row.ChangeRequestId
          AttributeName = row.AttributeName
          AiValue = Option.defaultValue "" row.AI2Value
          AiSource = getValueSource row.AIDELookupCode
          AideValue = Option.defaultValue "" row.AIDEValue
          AideSource = getValueSource row.AILookupCode
        }

    // ************************************************************************

    [<Literal>]
    let AssetChangeSchema = 
        "ChangeRequestId(int64),RequestStatus(string),\
         ChangeRequestType(string),AssetReference(string),\
         AI2AssetName(string),AI2CommonName(string),\
         AI2InstalledFromDate(date),AI2Manufacturer(string),\
         AI2Model(string),AI2HierarchyKey(string),\
         AI2AssetStatus(string),AI2LocationReference(string),\
         AideAssetName(string),AideCommonName(string),\
         AIDEInstalledFromDate(date),AIDEManufacturer(string),\
         AIDEModel(string),AI2HierarchyKey(string),\
         AIDEAssetStatus(string),AIDELocationReference(string),\
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

    let convertAssetChangeRow (row : AssetChangeRow) : AssetChange = 
        { ChangeRequestId = row.ChangeRequestId
          Reference = row.AssetReference
          AssetName = row.AI2AssetName
        }
