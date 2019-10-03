// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.AideReport


module PrintReport =
    
    open System.IO

    // Note there is a name clash, so open this before MarkdownDoc.Markdown
    // Possibly SLFormat.CommandOptions should not expose a function called text
    open SLFormat.CommandOptions

    open MarkdownDoc.Markdown
    open MarkdownDoc.Markdown.InlineHtml
    open MarkdownDoc.Markdown.RoseTree
    open MarkdownDoc.Markdown.CssColors
    open MarkdownDoc.Pandoc

    open AideSync.AideReport.Datatypes

    let headingTitle : string -> Markdown = 
        markdownText << doubleAsterisks << text

    let linkToTop : Markdown = 
        inlineLink "Back to top" "#top" None |> markdownText


    // ************************************************************************
    // Change scheme table

    let numberOfChangeRequests (scheme : ChangeScheme) : int =
        scheme.ChangeRequests 
            |> List.map (fun x -> x.Info.ChangeRequestId)
            |> List.distinct
            |> List.length

    /// Change Scheme Table 
    let changeSchemeSummaryTable (scheme : ChangeScheme) : Markdown =
        let specs = 
            [ { ColumnSpec.Width = 20 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ; { ColumnSpec.Width = 40 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ]
        let makeRow (name : string) (value : Text) : TableRow = 
            [ doubleAsterisks (name |> text) |> markdownText ; value |> markdownText ]
        let rows : TableRow list= 
            [ makeRow "Scheme Code"         (text scheme.Info.SchemeCode)
            ; makeRow "Scheme Id"           (int64Md scheme.Info.SchemeId)
            ; makeRow "Scheme Name"         (text scheme.Info.Name)
            ; makeRow "Solution Provider"   (text scheme.Info.SolutionProvider)
            ; makeRow "No. of Change Requests" (numberOfChangeRequests scheme |> int32Md)
            ]
        makeTableWithoutHeadings specs rows |> gridTable


    // ************************************************************************
    // Change requests summary

    /// Change Request Table 
    let changeRequestInfosTable (requests : ChangeRequest list) : Markdown option = 
        let headings =
            [ alignLeft 25 (headingTitle "Change Request Id")
            ; alignLeft 20 (headingTitle "Asset Name")
            ; alignLeft 40 (headingTitle "Common Name")
            ; alignLeft 25 (headingTitle "Status")
            ; alignLeft 30 (headingTitle "Comment")
            ]

        let makeRow (request : ChangeRequest) : TableRow= 
            let name = request.Info.ChangeRequestId.ToString()
            let assetName,commonName = 
                match request.StructureChange with
                | HierarchyNode(root : StructureNode,_) -> 
                    root.ShortName, root.CommonName
            [ inlineLink name ("#cr" + name) None           |> markdownText
            ; assetName                 |> text             |> markdownText 
            ; commonName                |> text             |> markdownText 
            ; text request.Info.Status                      |> markdownText
            ; request.Info.Comment      |> text             |> markdownText 
            ]
        
        let rows = requests |> List.map makeRow 
    
        match requests with
        | [] -> None
        | _ -> makeTableWithHeadings headings rows |> gridTable |> Some


    let changeRequestContentsTable (changeRequests : ChangeRequest list) : Markdown =
        match changeRequestInfosTable changeRequests with
        | None -> asterisks (text "No change requests")  |> h3
        | Some table -> table

    // ************************************************************************
    // Individual change request details

    /// Individual Change Request section header
    let changeRequestSectionHeader (changeRequestInfo : ChangeRequestInfo) : Markdown = 
        let requestId =  changeRequestInfo.ChangeRequestId   

        let title = 
            let refname = sprintf "cr%i" requestId
            htmlAnchorId refname (text "Change request" ^+^ int64Md requestId)

        h2 title
            ^!!^ markdownText (text "Request status:" ^+^ text changeRequestInfo.Status)
            ^!!^ markdownText (text "Request time:" ^+^ iso8601DateTimeMd changeRequestInfo.RequestTime)
            ^!!^ markdownText (text "Request type:" ^+^ text changeRequestInfo.RequestType)
            ^!!^ markdownText (text "Comment:" ^+^ text changeRequestInfo.Comment)

    let nodespan (colourName : string) (extraAttrs : HtmlAttrs) (body : Text) : Text = 
        htmlSpan (attrStyle [backgroundColor colourName] :: extraAttrs) body

    let drawLabel (isRoot : bool) (item : StructureNode)  : Markdown = 
        let makeLabelL (item : AiFlocNode) : Text = 
            text <| if isRoot then item.CommonName else item.ShortName
        let makeLabelR (item : AideFlocNode) : Text = 
            text <| if isRoot then item.CommonName else item.ShortName

        match item with
        | Deleted s -> 
            let title = htmlAttr "title" (sprintf "Delete '%s'" s.CommonName)
            nodespan lightCoral [title] (makeLabelL s) |> markdownText
        | Common(s1,s2,changes) -> 
            // makeLabelR s2 |> markdownText
            if changes.HasChanges then
                let title = htmlAttr "title" "Attributes Changed"
                nodespan gold [title] (makeLabelR s2)  |> markdownText
            else
                makeLabelR s2 |> markdownText
        | Added(s,changes) -> 
            let title = htmlAttr "title" (sprintf "Add '%s'" s.CommonName)
            nodespan paleGreen [title] (makeLabelR s) |> markdownText


    let changeRequestTree (hierarchy : Hierarchy<StructureNode>) : Markdown = 
        hierarchy.ToMarkdownTree() 
            |> RoseTree.mapTree2 (drawLabel true) (drawLabel false)
            |> RoseTree.drawTree


    let changeRequestDetails (changeRequest : ChangeRequest) : Markdown = 
        changeRequestSectionHeader changeRequest.Info
            ^!!^ changeRequestTree changeRequest.StructureChange
            ^!!^ linkToTop

    // ************************************************************************
    // Full report


    let makeFullReport (changeScheme : ChangeScheme) : Markdown = 
 
        h1 (htmlAnchorId "top" (text "AIDE Change Scheme"))
            ^!!^ changeSchemeSummaryTable changeScheme
            ^!!^ changeRequestContentsTable changeScheme.ChangeRequests
            ^!!^ vsep (List.map changeRequestDetails changeScheme.ChangeRequests)


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


    let writeFullReport (changeScheme : ChangeScheme) 
                        (pandocOpts : PandocOptions)
                        (outputHtmlFile : string) : Result<unit, string> = 
        let doc = makeFullReport changeScheme
        writeMarkdownReport doc "Aide Change Scheme Report" pandocOpts outputHtmlFile

