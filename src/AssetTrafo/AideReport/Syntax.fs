// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AideReport


module Syntax =
    
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
        

    type AttributeChange = 
        { AttributeName : string
          AiValue : string
          AiSource : ValueSource
          AideValue : string
          AideSource : ValueSource
        }
        