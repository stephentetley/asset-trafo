// Copyright (c) Stephen Tetley 2019
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

    /// Add to markdown-doc?
    let commaSpaceSep (texts : Text list) : Text = 
        match texts with 
        | [] -> Text.empty
        | [d1] -> d1
        | d1 :: rest -> List.fold (fun ac d -> ac ^^ character ',' ^+^ d) d1 rest
        
        


    let makeTitle1 (assetChanges : AssetChange list) : Text = 
        let ids = 
            assetChanges |> List.map (fun x -> x.ChangeRequestId)
        let display xs = 
            let docs = List.map int64Doc xs
            if docs.Length > 3 then
                List.take 3 docs @ [text "..."] |> commaSpaceSep
            else
                docs |> commaSpaceSep
                
        match ids with
        | [] -> text "AIDE Changes"
        | [x] -> text "AIDE Changes - Change Request:" ^+^ int64Doc x
        | _ -> text "AIDE Changes - Change Requests:" ^+^ display ids


    let makeMarkdownReport (assetChanges : AssetChange list) 
                            (attrChanges : AttributeChange list) : Markdown = 
        h1 (makeTitle1 assetChanges) 
            ^!!^ h2 (text "Asset")
            ^!!^ gridTable (assetHeaderMdTable assetChanges)
            ^!!^ h2 (text "Asset Changes")
            ^!!^ concatMarkdown (List.map assetPropChangesMdTable assetChanges)
            ^!!^ h2 (text "Attribute Changes")
            ^!!^ gridTable (attributeChangesMdTable attrChanges)


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


    let generateChangesReport (assetChangesCsvFile : string) 
                              (attributeChangesCsvFile : string) 
                              (pandocOpts : PandocOptions)
                              (outputHtmlFile : string) : Result<unit, string> = 
        let assetRows1 = 
            readAssetChangeExport assetChangesCsvFile
                |> Result.map (Seq.map convertAssetChangeRow >> Seq.toList)
    
        let attrRows1 = 
            readAttributeChangeExport attributeChangesCsvFile
                |> Result.map (Seq.map convertAttributeChangeRow >> Seq.toList)
    
        match assetRows1, attrRows1 with 
        | Ok assetRows, Ok attrRows -> 
            let doc = makeMarkdownReport assetRows attrRows
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
                    (Some "Changes Report")
                    pandocOpts
            match retCode with
            | Ok i -> printfn "Return code: %i" i ; Ok ()
            | Error msg -> Error msg
        | Error msg, _ -> Error msg
        | _, Error msg -> Error msg
        
    