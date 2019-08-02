﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AideChangeReport


module ChangeReport =

    open System.IO

    open SLFormat.CommandOptions

    open MarkdownDoc
    open MarkdownDoc.Markdown
    open MarkdownDoc.Pandoc

    open AssetTrafo.AideChangeReport.Syntax
    open AssetTrafo.AideChangeReport.ReadCsv

    /// Markdown report 

    let alignLeft (width : int) : ColumnSpec = 
        { Width = width; Alignment = Alignment.AlignLeft }


    let valueCell (value : string) : Markdown.Table.TableCell = 
        match value with
        | null | "NULL" -> "" |> text |> paraText 
        |_ -> value |> text |> paraText 
    
    
    
    let attributeChangeMdRow (attrChange : AttributeChange) : Markdown.Table.TableRow = 
        [ attrChange.AttributeName      |> text     |> paraText 
        ; valueCell attrChange.AiValue 
        ; valueCell attrChange.AideValue
        ]



    let attributeChangesMdTable (attrChanges : AttributeChange list) : Markdown.Table.Table = 
        let (rows : TableRow list) = 
            attrChanges |> List.map attributeChangeMdRow
        let specs = [ alignLeft 30; alignLeft 30; alignLeft 45; alignLeft 45 ]
        let headers = [ "Attributes"; "AI Value"; "AIDE Value"] 
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

    let assetPropertyMdRow (assetProperty : AssetProperty) : Markdown.Table.TableRow option = 
        if assetProperty.HasChanged then
            [ assetProperty.PropertyName    |> text |> paraText 
            ; assetProperty.AiValue         |> text |> paraText 
            ; assetProperty.AideValue       |> text |> paraText 
            ] |> Some

        else
            None

    let assetPropChangesMdTable (assetChange : AssetChange) : Markdown = 
        if  assetChange.HasChangedProperties then
            let (rows : TableRow list) = 
                assetChange.AssetProperties 
                    |> List.choose assetPropertyMdRow
            let specs = [ alignLeft 30; alignLeft 35; alignLeft 35; alignLeft 35 ]
            let headers = [ "Change Request Id"; "Name"; "AI2 Value"; "AIDE Value" ] 
                            |> List.map ( paraText << doubleAsterisks << text)
            makeTable specs headers rows |> gridTable
        else
            Markdown.emptyMarkdown

    /// Add to markdown-doc?
    let commaSpaceSep (texts : Text list) : Text = 
        match texts with 
        | [] -> Text.empty
        | [d1] -> d1
        | d1 :: rest -> List.fold (fun ac d -> ac ^^ character ',' ^+^ d) d1 rest
        
        


    let makeTitle1 (changeRequestIds : int64 list) : Text = 
        let display xs = 
            let docs = List.map int64Doc xs
            if docs.Length > 3 then
                List.take 3 docs @ [text "..."] |> commaSpaceSep
            else
                docs |> commaSpaceSep
                
        match changeRequestIds with
        | [] -> text "AIDE Changes"
        | [x] -> text "AIDE Changes - Change Request:" ^+^ int64Doc x
        | xs -> text "AIDE Changes - Change Requests:" ^+^ display xs


    let makeMarkdownReport (changeRequests : ChangeRequest list) : Markdown = 
        let requestIds = changeRequests |> List.map (fun x -> x.ChangeRequestId)
        let makeBody1 (changeRequest : ChangeRequest) = 
            h2 (text "Asset")
            ^!!^ gridTable (assetHeaderMdTable changeRequest.AssetChanges)
            ^!!^ h2 (text "Asset Changes: change request" ^+^ int64Doc changeRequest.ChangeRequestId )
            ^!!^ concatMarkdown (List.map assetPropChangesMdTable changeRequest.AssetChanges)
            ^!!^ h2 (text "Attribute Changes: change request" ^+^ int64Doc changeRequest.ChangeRequestId )
            ^!!^ gridTable (attributeChangesMdTable changeRequest.AttributeChanges)

        h1 (makeTitle1 requestIds) 
            ^!!^ Markdown.concatMarkdown (List.map makeBody1 changeRequests)


    let pandocHtmlDefaults (pathToCss : string) : PandocOptions = 
        let highlightStyle = argument "--highlight-style" &= argValue "tango"
        let selfContained = argument "--self-contained"
        /// Github style is nicer for tables than Tufte
        /// Note - body width has been changed on both stylesheets
        let css = argument "--css" &= doubleQuote pathToCss
        { Standalone = true
          InputExtensions = []
          OutputExtensions = []
          OtherOptions = [ css; highlightStyle; selfContained ]  }


    let generateChangesReport (changesSource : ChangesSourceFiles) 
                              (pandocOpts : PandocOptions)
                              (outputHtmlFile : string) : Result<unit, string> = 
         match readChangesSource changesSource with 
        | Ok changes -> 
            let doc = makeMarkdownReport changes
            let mdFileFull = Path.ChangeExtension(outputHtmlFile, "md") 
            let mdFileName = Path.GetFileName(mdFileFull)
            let htmlFileName = Path.GetFileName(outputHtmlFile)
            let outputDirectory = Path.GetDirectoryName(outputHtmlFile)
            doc.Save(columnWidth = 140, outputPath = mdFileFull)
            let retCode = 
                runPandocHtml5 
                    true 
                    outputDirectory 
                    mdFileName
                    htmlFileName
                    (Some "Aide Changes Report")
                    pandocOpts
            match retCode with
            | Ok i -> printfn "Return code: %i" i ; Ok ()
            | Error msg -> Error msg
        | Error msg -> Error msg
        
    