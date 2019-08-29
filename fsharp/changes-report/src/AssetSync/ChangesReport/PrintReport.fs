// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module PrintReport =
    
    open System.IO

    // Note there is a name clash, so open this before MarkdownDoc.Markdown
    // Possibly SLFormat.CommandOptions should not expose a function called text
    open SLFormat.CommandOptions

    open MarkdownDoc.Markdown
    open MarkdownDoc.Markdown.InlineHtml
    open MarkdownDoc.Pandoc
    

    open AssetSync.Base.Addendum
    open AssetSync.ChangesReport.Datatypes


    let headingTitle : string -> Markdown = 
        markdownText << doubleAsterisks << text

    let linkToTop : Markdown = 
        inlineLink "Back to top" "#top" None |> markdownText


    // ************************************************************************
    // Change scheme

    /// Change Scheme Table 
    let changeSchemeInfoTable (info : ChangeSchemeInfo) : Markdown =
        let specs = 
            [ { ColumnSpec.Width = 20 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ; { ColumnSpec.Width = 40 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ]
        let makeRow (name : string) (value : Text) : TableRow = 
            [ doubleAsterisks (name |> text) |> markdownText ; value |> markdownText ]
        let rows : TableRow list= 
            [ makeRow "Scheme Code"         (text info.Code)
            ; makeRow "Scheme Id"           (int64Md info.SchemeId)
            ; makeRow "Scheme Name"         (text info.Name)
            ; makeRow "Solution Provider"   (text info.SolutionProvider)
            ]
        makeTableWithoutHeadings specs rows |> gridTable


    // ************************************************************************
    // Changes request

    /// Change Request Table 
    let changeRequestInfosTable (infos : ChangeRequestInfo list) : Markdown option = 
        let headings =
            [ alignLeft 30 (headingTitle "Change Request Id")
            ; alignLeft 28 (headingTitle "Status")
            ; alignLeft 28 (headingTitle "Request Time")
            
            ; alignLeft 32 (headingTitle "Comment")
            ]

        let makeRow (info : ChangeRequestInfo) : TableRow= 
            let name = info.ChangeRequestId.ToString()
            [ inlineLink name ("#cr" + name) None   |> markdownText 
            ; text info.Status                      |> markdownText
            ; iso8601DateTimeMd info.RequestTime    |> markdownText 
            ; info.Comment      |> text             |> markdownText 
            ]
        
        let rows = infos |> List.map makeRow 
    
        match infos with
        | [] -> None
        | _ -> makeTableWithHeadings headings rows |> gridTable |> Some


    let changeRequestInfosSection (infos : ChangeRequestInfo list) : Markdown =
        match changeRequestInfosTable infos with
        | None -> asterisks (text "No change requests")  |> h3
        | Some table -> table


    /// Individual Change Request Section
    let changeRequestSectionHeader (changeRequestInfo : ChangeRequestInfo) = 
        let requestId =  changeRequestInfo.ChangeRequestId   

        let title = 
            let refname = sprintf "cr%i" requestId
            htmlIdAnchor refname (text "Change request" ^+^ int64Md requestId)

        h2 title
            ^!!^ markdownText (text "Request status:" ^+^ text changeRequestInfo.Status)
            ^!!^ markdownText (text "Request time:" ^+^ iso8601DateTimeMd changeRequestInfo.RequestTime)
            ^!!^ markdownText (text "Request type:" ^+^ text changeRequestInfo.RequestType)
            ^!!^ markdownText (text "Comment:" ^+^ text changeRequestInfo.Comment)


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
    
    let attributeChangesSection (attributeChanges : AttributeChange list) : Markdown = 
        match attributeChangesTable attributeChanges with
        | None -> asterisks (text "No attribute changes")  |> h3
        | Some table -> 
            h3 (text "Attribute Changes" )
                ^!!^ table

    // ************************************************************************
    // Attribute changes

   
    let repeatedAttributeChangeRow (repAttrChange : RepeatedAttributeChange) : Table.TableRow = 
        [ repAttrChange.Reference               |> text     |> markdownText
        ; repAttrChange.AssetName               |> text     |> markdownText
        ; repAttrChange.AttributeSetName        |> text     |> markdownText 
        ; repAttrChange.RepeatedAttributeName   |> text     |> markdownText 
        ; attributeValue repAttrChange.AiValue              |> markdownText 
        ; attributeValue repAttrChange.AideValue            |> markdownText 
        ]

    let repeatedAttributeChangesTable (repAttrChanges : RepeatedAttributeChange list) : Markdown option = 
        let headings =
            [ alignLeft 30 (headingTitle "Asset Reference")
            ; alignLeft 40 (headingTitle "Asset Name")
            ; alignLeft 40 (headingTitle "Attribute Set Name")
            ; alignLeft 40 (headingTitle "Attribute Name")
            ; alignLeft 30 (headingTitle "AI Value")
            ; alignLeft 30 (headingTitle "AIDE Value")
            ]
        let (rows : TableRow list) = repAttrChanges |> List.map repeatedAttributeChangeRow 
        match rows with
        | [] -> None
        | _ -> makeTableWithHeadings headings rows |> gridTable |> Some
    
    let repeatedAttributeChangesSection (repeatedAttributeChanges : RepeatedAttributeChange list) : Markdown = 
        match repeatedAttributeChangesTable repeatedAttributeChanges with
        | None -> asterisks (text "No repeated attribute changes")  |> h3
        | Some table -> 
            h3 (text "Repeated Attribute Changes" )
                ^!!^ table

    // ************************************************************************
    // Asset "property" changes


    let assetPropertyChangeRow (reference : string) 
                                (assetName : string) 
                                (assetProperty : AssetProperty) : Table.TableRow = 
        [ reference                     |> text |> markdownText
        ; assetName                     |> text |> markdownText
        ; assetProperty.PropertyName    |> text |> markdownText 
        ; assetProperty.AiValue         |> text |> markdownText 
        ; assetProperty.AideValue       |> text |> markdownText 
        ]

    let assetPropertyChangesTable (assetChanges : AssetChange list) : Markdown option = 
        
        let headings = 
            [ alignLeft 30 (headingTitle "Asset Reference")
            ; alignLeft 40 (headingTitle "Asset Name")
            ; alignLeft 35 (headingTitle "Asset Property")
            ; alignLeft 35 (headingTitle "AI2 Value")
            ; alignLeft 35 (headingTitle "AIDE Value")
            ]
        
        let makeRows1 (assetChange : AssetChange)  = 
            assetChange.AssetProperties 
                |> List.map (assetPropertyChangeRow assetChange.Reference assetChange.AiAssetName)

        let rows = assetChanges |> List.map makeRows1 |> List.concat
        match rows with
        | [] -> None
        | _ -> makeTableWithHeadings headings rows |> gridTable |> Some


    let assetPropertyChangesSection (assetChanges : AssetChange list) : Markdown =
        match assetPropertyChangesTable assetChanges with
        | None -> asterisks (text "No asset property changes")  |> h3
        | Some table -> 
            h3 (text "Asset Property Changes" )
                ^!!^ table

    // ************************************************************************
    // Build the document

    let makeChangeRequest1 (changeRequest : ChangeRequest) : Markdown = 
        changeRequestSectionHeader (changeRequest.Info)
            ^!!^ assetPropertyChangesSection changeRequest.AssetChanges
            ^!!^ attributeChangesSection changeRequest.AttributeChanges
            ^!!^ repeatedAttributeChangesSection changeRequest.RepeatedAttributeChanges
            ^!!^ linkToTop

    let makeChangeRequestsReport (changeRequests : ChangeRequest list) : Markdown = 
        let requestInfos = changeRequests |> List.map (fun x -> x.Info)

        h1 (htmlIdAnchor "top" (text "AIDE Change Requests"))
            ^!!^ changeRequestInfosSection requestInfos
            ^!!^ vsep (List.map makeChangeRequest1 changeRequests)
      
    let makeChangeSchemeReport (changeScheme : ChangeScheme) : Markdown = 
        let requestInfos = changeScheme.ChangeRequests |> List.map (fun x -> x.Info)

        h1 (htmlIdAnchor "top" (text "AIDE Change Scheme"))
            ^!!^ changeSchemeInfoTable changeScheme.Info
            ^!!^ changeRequestInfosSection requestInfos
            ^!!^ vsep (List.map makeChangeRequest1 changeScheme.ChangeRequests)

    // ************************************************************************
    // Invoking Pandoc

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


    let writeMarkdownReport (doc : Markdown) 
                            (pageTitle : string)
                            (pandocOpts : PandocOptions)
                            (outputHtmlFile : string) : Result<unit, string> = 
        
        let mdFileAbsPath = Path.ChangeExtension(outputHtmlFile, "md") 
        let mdFileName = Path.GetFileName(mdFileAbsPath)
        let htmlFileName = Path.GetFileName(outputHtmlFile)
        let outputDirectory = Path.GetDirectoryName(outputHtmlFile)
        writeMarkdown 140 doc mdFileAbsPath
        let retCode = 
            runPandocHtml5 
                true 
                outputDirectory 
                mdFileName
                htmlFileName
                (Some pageTitle)
                pandocOpts
        match retCode with
        | Ok i -> printfn "Return code: %i" i ; Ok ()
        | Error msg -> Error msg

        
    let writeChangeRequestsReport (changeRequests : ChangeRequest list) 
                                  (pandocOpts : PandocOptions)
                                  (outputHtmlFile : string) : Result<unit, string> = 
        let doc = makeChangeRequestsReport changeRequests
        writeMarkdownReport doc "Aide Change Requests Report" pandocOpts outputHtmlFile
    
    let writeChangeSchemeReport (changeScheme : ChangeScheme) 
                                (pandocOpts : PandocOptions)
                                (outputHtmlFile : string) : Result<unit, string> = 
        let doc = makeChangeSchemeReport changeScheme
        writeMarkdownReport doc "Aide Change Scheme Report" pandocOpts outputHtmlFile