// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module Datatypes =
    
    
    


    type AttributeValue = 
        | Literal of string
        | Lookup of resolved : string
        member x.Value 
            with get() : string = 
                match x with
                | Literal s -> s
                | Lookup s -> s

        
    /// This represents a change on an attribute.
    /// The report on the system that we use for input only 
    /// shows changes. If an attribute is unchanged there will 
    /// be no record.
    type AttributeChange = 
        { AssetName : string
          Reference : string
          AttributeName : string
          AiValue : AttributeValue
          AideValue : AttributeValue
        }
        /// Note this should always be true as we only extract changed 
        /// records from the master database
        member x.HasChanged with get () : bool = x.AideValue <> x.AiValue

    type RepeatedAttributeChange = 
        { AssetName : string
          Reference : string
          RepeatedAttributeName : string
          AttributeSetName : string
          AiValue : AttributeValue
          AideValue : AttributeValue
        } 
        /// Note this should always be true as we only extract changed 
        /// records from the master database
        member x.HasChanged with get () : bool = x.AideValue <> x.AiValue

    /// Assets have "properties" { name, common name, manufacturer, 
    /// grid ref... } that are stored inline with the "Asset record". 
    /// Because they are stored inline, "properties" are diffferent
    /// to attributes that are stored in a separate table with a foreign
    /// key back to the asset.
    /// The input report lists both AI and AIDE property values regardless
    /// of whether the value has changed in AIDE.
    type AssetProperty = 
        { PropertyName : string
          AiValue : string
          AideValue : string
        }
        member x.HasChanged with get () : bool = x.AideValue <> x.AiValue

    type AssetChange = 
        { Reference : string
          AiAssetName : string
          AiCommonName : string
          AssetProperties : AssetProperty list
        }
        member x.HasChangedProperties 
            with get () : bool = 
                x.AssetProperties |> List.exists (fun prop -> prop.HasChanged)


    type ChangeRequestInfo = 
        { ChangeRequestId : int64
          RequestType : string
          Status : string
          Comment: string
          RequestTime : System.DateTime
        }


    type ChangeRequest = 
        { Info : ChangeRequestInfo
          AssetChanges : AssetChange list
          AttributeChanges : AttributeChange list
          RepeatedAttributeChanges : RepeatedAttributeChange list
        }


    type ChangeSchemeInfo = 
        { SchemeId : int64
          Code : string
          Name : string
          SolutionProvider : string
        }

    type ChangeScheme = 
        { Info : ChangeSchemeInfo
          ChangeRequests : ChangeRequest list
        }