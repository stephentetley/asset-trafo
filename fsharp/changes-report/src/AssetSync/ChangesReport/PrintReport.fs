// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module PrintReport =
    
    open MarkdownDoc.Markdown
    open MarkdownDoc.Pandoc

    open AssetSync.ChangesReport.Addendum
    open AssetSync.ChangesReport.Datatypes


    let headingTitle : string -> Markdown = 
        markdownText << doubleAsterisks << text

    // ************************************************************************
    // Attribute changes

    let attributeValue (attrValue : AttributeValue) : Text = 
        text attrValue.Value
   
    let attributeChangeRow (attrChange : AttributeChange) : Table.TableRow = 
        [ attrChange.Reference          |> text     |> markdownText
        ; attrChange.AssetName          |> text     |> markdownText
        ; attrChange.AttributeName      |> text     |> markdownText 
        ; attributeValue attrChange.AiValue         |> markdownText 
        ; attributeValue attrChange.AideValue       |> markdownText 
        ]

    let attributeChangesTable (attrChanges : AttributeChange list) : Markdown option = 
        let headings =
            [ alignLeft 30 (headingTitle "Asset Reference")
            ; alignLeft 40 (headingTitle "Asset Name")
            ; alignLeft 30 (headingTitle "Attribute Name")
            ; alignLeft 45 (headingTitle "AI Value")
            ; alignLeft 45 (headingTitle "AIDE Value")
            ]
        let (rows : TableRow list) = attrChanges |> List.map attributeChangeRow 
        match rows with
        | [] -> None
        | _ -> makeTableWithHeadings headings rows |> gridTable |> Some
    
    // ************************************************************************
    // Asset 'property' changes


    let assetPropertyChangeRow (reference : string) 
                                (assetName : string) 
                                (assetProperty : AssetProperty) : Table.TableRow = 
        [ reference                     |> text |> markdownText
        ; assetName                     |> text |> markdownText
        ; assetProperty.PropertyName    |> text |> markdownText 
        ; assetProperty.AiValue         |> text |> markdownText 
        ; assetProperty.AideValue       |> text |> markdownText 
        ]

    let assetPropertyChangesTable (reference : string) 
                                    (assetName : string) 
                                    (assetChanges : AssetProperty list) : Markdown option = 
        
        let headings = 
            [ alignLeft 30 (headingTitle "Asset Reference")
            ; alignLeft 35 (headingTitle "Asset Name")
            ; alignLeft 35 (headingTitle "Asset Property")
            ; alignLeft 35 (headingTitle "AI2 Value")
            ; alignLeft 35 (headingTitle "AIDE Value")
            ]
        let rows = assetChanges |> List.map (assetPropertyChangeRow reference assetName)
        match rows with
        | [] -> None
        | _ -> makeTableWithHeadings headings rows |> gridTable |> Some