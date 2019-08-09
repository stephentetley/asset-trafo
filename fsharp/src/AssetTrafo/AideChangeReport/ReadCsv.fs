// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AideChangeReport


module ReadCsv =
    
    open FSharp.Core
    open FSharp.Data

    open AssetTrafo.AideChangeReport.Syntax

    [<Literal>]
    let AttributeChangeSchema = 
        "ChangeRequestId(int64),RequestStatus(string),\
         Reference(string),AssetName(string),\
         AssetCommonName(string),\
         AttributeName(string),AiValue(string option),\
         AiLookupValue(string option),AiLookupCode(int64 option),\
         AideValue(string option),AideLookupValue(string option),\
         AideLookupCode(int64 option),ChangeRequestTime(date)"


    [<Literal>]
    let AttributeChangeSample = 
        "10000,Submitted,ABC01,ASSET_SHORT_NAME,\
        ASSET_COMMON_NAME,ATTR_NAME,\
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

    let convertAttributeChangeRow (row : AttributeChangeRow) : int64 * AttributeChange = 
        let attrChange = 
            { AssetName = row.AssetName
              Reference = row.Reference
              AttributeName = row.AttributeName
              AiValue = getValue row.AiValue row.AiLookupCode row.AiLookupValue
              AiSource = getValueSource row.AideLookupCode
              AideValue = getValue row.AideValue row.AideLookupCode row.AideLookupValue
              AideSource = getValueSource row.AiLookupCode
            }
        row.ChangeRequestId, attrChange

    // ************************************************************************

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

    let convertAssetChangeRow (row : AssetChangeRow) : int64 * AssetChange = 
        let assetChange = 
            { Reference = row.AssetReference
              AiAssetName = row.AiAssetName
              AiCommonName = row.AiCommonName
              AssetProperties = readProperties row
            }
        row.ChangeRequestId, assetChange

    type MValue= string * System.DateTime * AssetChange list * AttributeChange list

    type InterimMap =  Map<int64, MValue>

    let pushL (ix : int64) 
              (status: string) 
              (requestTime : System.DateTime) 
              (change : AssetChange) 
              (imap : InterimMap) : InterimMap = 
        match Map.tryFind ix imap with
        | None -> Map.add ix (status, requestTime, [change], []) imap
        | Some (s, dt, xs, ys) -> Map.add ix (s, dt, change :: xs, ys) imap
 
 
    let pushR (ix : int64)
              (status: string) 
              (requestTime : System.DateTime)  
              (change : AttributeChange) 
              (imap : InterimMap) : InterimMap = 
        match Map.tryFind ix imap with
        | None -> Map.add ix (status, requestTime, [], [change]) imap
        | Some (s, dt, xs, ys) -> Map.add ix (s, dt, xs, change :: ys) imap


    // Build ChangeRequests from tables of asset and attribute changes
    let private build1 (assetChanges : AssetChangeRow seq) 
                        (attrChanges : AttributeChangeRow seq) : ChangeRequest list =         
        let attrsMap : InterimMap = 
            Seq.fold (fun st x -> 
                        let (ix, c1) = convertAssetChangeRow x
                        pushL ix x.RequestStatus x.ChangeRequestTime c1 st)
                    Map.empty assetChanges

        let attrsMap2 = 
            Seq.fold (fun st x -> 
                        let (ix, c1) = convertAttributeChangeRow x
                        pushR ix x.RequestStatus x.ChangeRequestTime c1 st)
                        attrsMap attrChanges
        attrsMap2
            |> Map.toList 
            |> List.map (fun (ix, (s, dt, xs, ys)) -> 
                                { ChangeRequestId = ix
                                  RequestStatus = s
                                  RequestTime = dt
                                  AssetChanges = xs
                                  AttributeChanges = ys })
        

    let readChangesSource (sourceFiles : ChangesSourceFiles) : Result<ChangeRequest list , string> = 
        let optRead (readProc : string -> Result<'a seq, string>) (optfile : string option) = 
            match optfile with
            | None -> Ok Seq.empty
            | Some name -> readProc name
        match optRead readAssetChangeExport sourceFiles.AssetChangesCsv with
        | Error msg -> Error msg
        | Ok assets -> 
            match optRead readAttributeChangeExport sourceFiles.AttributeChangesCsv with
            | Error msg -> Error msg
            | Ok attrs -> 
                build1 assets attrs |> Ok
        

