// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AideReport


module ReadCsv =
    
    open FSharp.Data

    open AssetTrafo.AideReport.Syntax



    type AttributeChangeExport = 
        CsvProvider< Schema = @"AttributeName(string),AIValue(string),AIValueLookup(string),AIValueCode(string),AIDEValue(string),AIDEValueLookup(string),AIDEValueCode(string),ChangeRequestTime(date)"
                   , Sample = "attribute name 1,NULL,NULL,NULL,attribute value,NULL,NULL,2019-05-28T09:22:09"
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
        { AttributeName = row.AttributeName
          AiValue = row.AIValue
          AiSource = getValueSource row.AIValueCode
          AideValue = row.AIValue
          AideSource = getValueSource row.AIDEValueCode
        }