// Copyright (c) Stephen Tetley 2019
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
        { AttributeName : string
          AiValue : string
          AiSource : ValueSource
          AideValue : string
          AideSource : ValueSource
        }
     
     
    /// Markdown report 


    let valueCell (value : string) : Markdown.Table.TableCell = 
        match value with
        | null | "NULL" -> "" |> text |> paraText 
        |_ -> value |> text |> paraText 
    
    
    
    let attributeChangeMdRow (attrChange : AttributeChange) : Markdown.Table.TableRow = 
        [ attrChange.AttributeName |> text |> paraText 
        ; valueCell attrChange.AiValue 
        ; valueCell attrChange.AideValue
        ]

    let alignLeft (width : int) : ColumnSpec = 
        { Width = width; Alignment = Alignment.AlignLeft }

    let attributeChangesMdTable (attrChanges : AttributeChange list) : Markdown.Table.Table = 
        let (rows : TableRow list) = 
            attrChanges |> List.map attributeChangeMdRow
        let specs = [ alignLeft 30; alignLeft 45; alignLeft 45 ]
        let headers = ["Attributes"; "AI Value"; "AIDE Value"] 
                        |> List.map ( paraText << doubleAsterisks << text)
        makeTable specs headers rows

    let makeReport (attrChanges : AttributeChange list) : Markdown = 
        h1 (text "Changes") 
            ^!!^ gridTable (attributeChangesMdTable attrChanges)
