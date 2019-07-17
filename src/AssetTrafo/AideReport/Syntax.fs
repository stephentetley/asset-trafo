﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AideReport


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
        

    type AttributeChange = 
        { ChangeRequestId : int64
          AttributeName : string
          AiValue : string
          AiSource : ValueSource
          AideValue : string
          AideSource : ValueSource
        }

    type AssetChange = 
        { ChangeRequestId : int64
          Reference : string
          AssetName : string
          // OldParentName
          // NewParentName
        }
     
     
  