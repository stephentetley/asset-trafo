// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AideReport


module MarkDownReport =

    open MarkdownDoc
    open MarkdownDoc.Markdown
    open MarkdownDoc.Pandoc

    open AssetTrafo.AideReport.Syntax

    /// Markdown report 

    let alignLeft (width : int) : ColumnSpec = 
        { Width = width; Alignment = Alignment.AlignLeft }


    let valueCell (value : string) : Markdown.Table.TableCell = 
        match value with
        | null | "NULL" -> "" |> text |> paraText 
        |_ -> value |> text |> paraText 
    
    
    
    let attributeChangeMdRow (attrChange : AttributeChange) : Markdown.Table.TableRow = 
        [ attrChange.ChangeRequestId |> int64Doc |> paraText
        ; attrChange.AttributeName |> text |> paraText 
        ; valueCell attrChange.AiValue 
        ; valueCell attrChange.AideValue
        ]



    let attributeChangesMdTable (attrChanges : AttributeChange list) : Markdown.Table.Table = 
        let (rows : TableRow list) = 
            attrChanges |> List.map attributeChangeMdRow
        let specs = [ alignLeft 30; alignLeft 30; alignLeft 45; alignLeft 45 ]
        let headers = [ "Change Request Id"; "Attributes"; "AI Value"; "AIDE Value"] 
                        |> List.map ( paraText << doubleAsterisks << text)
        makeTable specs headers rows




    let assetChangeMdRow (assetChange : AssetChange) : Markdown.Table.TableRow = 
        [ assetChange.ChangeRequestId |> int64Doc |> paraText 
        ; assetChange.Reference |> text |> paraText 
        ; assetChange.AssetName |> text |> paraText 
        ]

    let assetChangesMdTable (assetChanges : AssetChange list) : Markdown.Table.Table = 
        let (rows : TableRow list) = 
            assetChanges |> List.map assetChangeMdRow
        let specs = [ alignLeft 30; alignLeft 30; alignLeft 45 ]
        let headers = [ "Change Request Id"; "Reference"; "Asset Name"] 
                        |> List.map ( paraText << doubleAsterisks << text)
        makeTable specs headers rows

    let makeReport (assetChanges : AssetChange list) 
                   (attrChanges : AttributeChange list) : Markdown = 
        h1 (text "Changes") 
            ^!!^ gridTable (assetChangesMdTable assetChanges)
            ^!!^ gridTable (attributeChangesMdTable attrChanges)
