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
        [ assetChange.Reference     |> text |> paraText 
        ; assetChange.AiAssetName   |> text |> paraText 
        ; assetChange.AiCommonName  |> text |> paraText 
        ]

    let assetHeaderMdTable (assetChanges : AssetChange list) : Markdown.Table.Table = 
        let (rows : TableRow list) = 
            assetChanges |> List.map assetChangeMdRow
        let specs = [ alignLeft 30; alignLeft 30; alignLeft 45 ]
        let headers = [ "Reference"; "Asset Name (AI2)"; "Common Name (AI2)" ] 
                        |> List.map ( paraText << doubleAsterisks << text)
        makeTable specs headers rows

    let assetPropertyMdRow (changeRequestId : int64) 
                            (assetProperty : AssetProperty) : Markdown.Table.TableRow option = 
        if assetProperty.HasChanged then
            [ changeRequestId               |> int64Doc |> paraText 
            ; assetProperty.PropertyName    |> text |> paraText 
            ; assetProperty.AiValue         |> text |> paraText 
            ; assetProperty.AideValue       |> text |> paraText 
            ] |> Some

        else
            None

    let assetPropChangesMdTable (assetChange : AssetChange) : Markdown = 
        if  assetChange.HasChangedProperties then
            let (rows : TableRow list) = 
                assetChange.AssetProperties 
                    |> List.choose (assetPropertyMdRow assetChange.ChangeRequestId)
            let specs = [ alignLeft 30; alignLeft 35; alignLeft 35; alignLeft 35 ]
            let headers = [ "Change Request Id"; "Name"; "AI2 Value"; "AIDE Value" ] 
                            |> List.map ( paraText << doubleAsterisks << text)
            makeTable specs headers rows |> gridTable
        else
            Markdown.emptyMarkdown

    let makeReport (assetChanges : AssetChange list) 
                   (attrChanges : AttributeChange list) : Markdown = 
        h1 (text "AIDE Changes") 
            ^!!^ h2 (text "Asset")
            ^!!^ gridTable (assetHeaderMdTable assetChanges)
            ^!!^ h2 (text "Asset Changes")
            ^!!^ concatMarkdown (List.map assetPropChangesMdTable assetChanges)
            ^!!^ h2 (text "Attribute Changes")
            ^!!^ gridTable (attributeChangesMdTable attrChanges)
