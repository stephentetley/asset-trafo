// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AideChangeReport


module Syntax =

    open MarkdownDoc
    open MarkdownDoc.Markdown
    open MarkdownDoc.Pandoc
    
    type ChangeRequestStatus = 
        | Pending
        | Submitted
        | Approved
        | Committed

    type ChangeRequestType = 
        | AIDE
        | Asset
        | Attribute
        | ChangeAssetType
        | Node
        | Relationship

    type ValueSource = 
        | Freetext
        | Lookup
        
    /// This represents a change on an attribute.
    /// The report on the system that we use for input only 
    /// shows changes. If an attribute is unchanged there will 
    /// be no record.
    type AttributeChange = 
        { ChangeRequestId : int64
          AttributeName : string
          AiValue : string
          AiSource : ValueSource
          AideValue : string
          AideSource : ValueSource
        }

    /// Assets have "properties" { name, common name, grid ref... }
    /// that are stored inline with the "Asset record". 
    /// Because they are stored inline, "properties" are diffferent
    /// to attributes that are stored in a separate table with a foreign
    /// key back to the asset.
    /// The input report lists both AIDE an AI property values regardless
    /// of whether the value has changed in AIDE.
    type AssetProperty = 
        { PropertyName : string
          AiValue : string
          AideValue : string
        }
        member x.HasChanged with get () : bool = x.AideValue <> x.AiValue


    type AssetChange = 
        { ChangeRequestId : int64
          Reference : string
          AiAssetName : string
          AiCommonName : string
          AssetProperties : AssetProperty list
        }
        member x.HasChangedProperties 
            with get () : bool = 
                x.AssetProperties |> List.exists (fun prop -> prop.HasChanged)
     
  