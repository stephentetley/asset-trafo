// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AideReport


module ReadCsv =
    
    open FSharp.Data

    open AssetTrafo.AideReport.Syntax



    type AttributeChangeExport = 
        CsvProvider< Schema = @"ChangeRequestId(int64),AttributeName(string),AIValue(string),AIValueLookup(string),AIValueCode(string),AIDEValue(string),AIDEValueLookup(string),AIDEValueCode(string),ChangeRequestTime(date)"
                   , Sample = "10000,attribute name 1,NULL,NULL,NULL,attribute value,NULL,NULL,2019-05-28T09:22:09"
                   , HasHeaders = true >

    type AttributeChangeRow = AttributeChangeExport.Row
    
    let readAttributeChangeExport(path:string) : seq<AttributeChangeRow> = 
        let table = AttributeChangeExport.Load(uri = path)
        table.Rows 

    let getValueSource (valueCode : string) : ValueSource = 
        match valueCode with
        | null | "NULL" -> Freetext
        | _ -> Lookup

    let convertAttributeChangeRow (row : AttributeChangeRow) : AttributeChange = 
        { ChangeRequestId = row.ChangeRequestId
          AttributeName = row.AttributeName
          AiValue = row.AIValue
          AiSource = getValueSource row.AIValueCode
          AideValue = row.AIDEValue
          AideSource = getValueSource row.AIDEValueCode
        }

    // ************************************************************************

    type AssetChangeExport = 
        CsvProvider< Schema = @"ChangeRequestId(int64),ChangeRequestType(string),ChangeRequestTime(date),ChangeRequestStatus(string),AssetReference(string),AssetName(string),AssetCommonName(string)"
                   , Sample = "148052,Attribute,2019-05-28T09:22:23,Submitted,ABC00123,NAME_1,LONG_NAME"
                   , HasHeaders = true >

    type AssetChangeRow = AssetChangeExport.Row
    
    let readAssetChangeExport(path:string) : seq<AssetChangeRow> = 
        let table = AssetChangeExport.Load(uri = path)
        table.Rows 

    let convertAssetChangeRow (row : AssetChangeRow) : AssetChange = 
        { ChangeRequestId = row.ChangeRequestId
          Reference = row.AssetReference
          AssetName = row.AssetName
        }
