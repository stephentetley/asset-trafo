// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport.Old


module ReadCsv =
    
    open FSharp.Core
    open FSharp.Data

    open AssetSync.ChangesReport.Old.ImportSchema
    open AssetSync.ChangesReport.Old.Syntax


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
            { AssetName = row.AiAssetName
              Reference = row.Reference
              AttributeName = row.AttributeName
              AiValue = getValue row.AiValue row.AiLookupCode row.AiLookupValue
              AiSource = getValueSource row.AideLookupCode
              AideValue = getValue row.AideValue row.AideLookupCode row.AideLookupValue
              AideSource = getValueSource row.AiLookupCode
            }
        row.ChangeRequestId, attrChange

    // ************************************************************************



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
        
    /// This is horrible...
    let readChangesSource (sourceFiles : ChangesSourceFiles) : Result<ChangeRequest list , string> = 
        let optRead (readProc : string -> Result<'a seq, string>) (optfile : string option) = 
            match optfile with
            | None -> Ok Seq.empty
            | Some name -> readProc name
        let readAssetProc path = 
            Result.map (fun (t1:AssetChangeTable) -> t1.Rows) (readAssetChangeExport path) 
        match optRead readAssetProc sourceFiles.AssetChangesCsv with
        | Error msg -> Error msg
        | Ok assets -> 
            let readAttrProc path = 
                Result.map (fun (t1:AttributeChangeTable) -> t1.Rows) (readAttributeChangeExport path) 
            match optRead readAttrProc sourceFiles.AttributeChangesCsv with
            | Error msg -> Error msg
            | Ok attrs -> 
                build1 assets attrs |> Ok
        

