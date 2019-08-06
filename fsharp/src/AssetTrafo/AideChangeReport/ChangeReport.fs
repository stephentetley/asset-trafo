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



    /// For MarkdownDoc

    let htmlIdAnchor (name : string) (body : Text) : Text = 
        rawtext (sprintf "<a id=\"%s\">" name) ^^ body ^^ rawtext "</a>"

    /// Print a DataTime. 
    /// The output uses FSharp's ToString() so it may be printed in 
    /// exponential notation.
    let dateTimeDoc (datetime : System.DateTime) (format : string) : Text = 
        datetime.ToString(format) |> text

    let iso8601DateTimeDoc (datetime : System.DateTime) : Text = 
        dateTimeDoc datetime "yyyy-MM-dd hh:mm:ss"


    /// Add to markdown-doc?
    let commaSpaceSep (texts : Text list) : Text = 
        match texts with 
        | [] -> Text.empty
        | [d1] -> d1
        | d1 :: rest -> List.fold (fun ac d -> ac ^^ character ',' ^+^ d) d1 rest


    /// Markdown report 

    let linkToTop () : Markdown = 
        inlineLink "Back to top" "#top" None |> markdownText

    let alignLeft (width : int) : ColumnSpec = 
        { Width = width; Alignment = Alignment.AlignLeft }


    let valueCell (value : string) : Markdown.Table.TableCell = 
        match value with
        | null | "NULL" -> "" |> text |> paraText 
        |_ -> value |> text |> paraText 
    


    /// Change Request Table
    let changeRequestInfosTable (infos : ChangeRequestInfo list) : Markdown = 
        let makeRow (requestId, status, requestTime) = 
            let name = requestId.ToString()
            [ inlineLink name ("#cr" + name) None |> paraText 
            ; text status |> paraText
            ; iso8601DateTimeDoc requestTime |> paraText 
            ]
        let specs = [ alignLeft 70 ; alignLeft 40; alignLeft 40 ]
        let headers = 
            [ "Change Request Id"; "Status"; "Request Time" ] 
                |> List.map ( paraText << doubleAsterisks << text)
        let rows = List.map makeRow infos
        
        match infos with
        | [] -> text "No change requests"  |> markdownText
        | _ -> makeTable specs headers rows |> gridTable
        
    
    // Make Attribute Changes table

    let attributeChangeRow (attrChange : AttributeChange) : Markdown.Table.TableRow = 
        [ attrChange.Reference          |> text     |> paraText
        ; attrChange.AssetName          |> text     |> paraText
        ; attrChange.AttributeName      |> text     |> paraText 
        ; valueCell attrChange.AiValue 
        ; valueCell attrChange.AideValue
        ]

    let attributeChangesTable (attrChanges : AttributeChange list) : Markdown = 

        let (rows : TableRow list) = 
            attrChanges |> List.map attributeChangeRow 

        let specs = [ alignLeft 30; alignLeft 40; alignLeft 30; alignLeft 45; alignLeft 45 ]
        
        let headers = [ "Reference"; "Asset"; "Attributes"; "AI Value"; "AIDE Value"] 
                        |> List.map ( paraText << doubleAsterisks << text)

        match attrChanges with
        | [] -> asterisks (text "No attribute changes")  |> h3
        | _ -> 
            h3 (text "Attribute Changes" )
            ^!!^ (makeTable specs headers rows |> gridTable)



    // Make "Asset Property" Changes table


    let assetPropertyChangeRow (reference : string) 
                               (assetName : string) 
                               (assetProperty : AssetProperty) : Markdown.Table.TableRow option = 
        if assetProperty.HasChanged then
            [ reference                     |> text |> paraText
            ; assetName                     |> text |> paraText
            ; assetProperty.PropertyName    |> text |> paraText 
            ; assetProperty.AiValue         |> text |> paraText 
            ; assetProperty.AideValue       |> text |> paraText 
            ] |> Some
        else None

    let assetPropertyChangeRows (assetChanges : AssetChange list) : Markdown.Table.TableRow list = 
        let changeRows1 (changes : AssetChange) : Markdown.Table.TableRow list = 
            if changes.HasChangedProperties then
                changes.AssetProperties 
                |> List.choose (assetPropertyChangeRow changes.Reference changes.AiAssetName)
            else []
        List.map changeRows1 assetChanges |> List.concat

    let assetPropertyChangesTable (assetChanges : AssetChange list) : Markdown = 
        let rows = assetPropertyChangeRows assetChanges
        let specs = [ alignLeft 30; alignLeft 35; alignLeft 35; alignLeft 35 ]
        let headers = [ "Asset"; "Name"; "AI2 Value"; "AIDE Value" ] 
                            |> List.map ( paraText << doubleAsterisks << text)

        match rows with
        | [] -> asterisks (text "No asset property changes")  |> h3
        | _ -> 
            h3 (text "Asset Property Changes" )
            ^!!^ (makeTable specs headers rows |> gridTable)
        
            

    let changeRequestSection (changeRequest : ChangeRequest) = 
        let requestId =  changeRequest.ChangeRequestId       

        let title = 
            let refname = sprintf "cr%i" requestId
            htmlIdAnchor refname (text "Change request" ^+^ int64Doc requestId)

        h2 title
        ^!!^ markdownText (text "Request status:" ^+^ text changeRequest.RequestStatus)
        ^!!^ markdownText (text "Request time:" ^+^ iso8601DateTimeDoc changeRequest.RequestTime)
        ^!!^ assetPropertyChangesTable changeRequest.AssetChanges
        ^!!^ attributeChangesTable changeRequest.AttributeChanges
        ^!!^ linkToTop ()

    let makeMarkdownReport (changeRequests : ChangeRequest list) : Markdown = 
        let requestInfos = changeRequests |> List.map (fun x -> x.RequestInfo)

        h1 (htmlIdAnchor "top" (text "AIDE Change Requests"))
            ^!!^ changeRequestInfosTable requestInfos
            ^!!^ Markdown.concatMarkdown (List.map changeRequestSection changeRequests)


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
        
    