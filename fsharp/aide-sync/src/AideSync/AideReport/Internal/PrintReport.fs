// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.AideReport.Internal


module PrintReport =
    
    open System.IO

    // Note there is a name clash, so open this before MarkdownDoc.Markdown
    

    open MarkdownDoc.Markdown
    open MarkdownDoc.Markdown.InlineHtml
    open MarkdownDoc.Markdown.RoseTree
    open MarkdownDoc.Markdown.CssColors
    open MarkdownDoc.Pandoc

    open AideSync.Base.Addendum
    open AideSync.Base.Common
    open AideSync.AideReport.Internal.Attributes
    open AideSync.AideReport.Internal.Datatypes

    

    let deletedRed : string = "#F1BFBF"

    let addedGreen : string = paleGreen

    let modifiedOrange : string = gold

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
            [ alignLeft 20 (headingTitle "Change Request Id")
            ; alignLeft 30 (headingTitle "Reference")
            ; alignLeft 30 (headingTitle "Asset Name")
            ; alignLeft 40 (headingTitle "Common Name")
            ; alignLeft 25 (headingTitle "Status")            
            ]

        let makeRow (request : ChangeRequest) : TableRow= 
            let name = request.Info.ChangeRequestId.ToString()
            let assetName = request.StructureChange.RootLabel.ShortName 
            let commonName = request.StructureChange.RootLabel.CommonName
            [ inlineLink name ("#cr" + name) None           |> markdownText
            ; request.StructureChange.RootLabel.Reference      |> text             |> markdownText 
            ; assetName                 |> text             |> markdownText 
            ; commonName                |> text             |> markdownText 
            ; text request.Info.Status                      |> markdownText
            ]
        
        let rows = requests |> List.map makeRow 
    
        match requests with
        | [] -> None
        | _ -> makeTableWithHeadings headings rows |> gridTable |> Some


    let changeRequestContentsTable (changeRequests : ChangeRequest list) : Markdown =
        match changeRequestInfosTable changeRequests with
        | None -> asterisks (text "No change requests")  |> h3
        | Some table -> table


    /// Change Attribute Table 
    let changeAttributeTable (changes : NodeChanges) : Markdown option =
        let headings =
            [ alignLeft 25 (headingTitle "Attribute Name")
            ; alignLeft 40 (headingTitle "AI Value")
            ; alignLeft 40 (headingTitle "AIDE Value")
            ; alignLeft 25 (headingTitle "Note")
            
            ]
        
        let makeRow (change : ValueChange) : TableRow= 
            [ change.FieldName      |> text        |> markdownText
            ; change.LeftValue      |> text        |> markdownText 
            ; change.RightValue     |> text        |> markdownText 
            ; change.Description    |> text        |> markdownText 
            ]
        
        let rows = changes |> attributeValueChanges |> List.map makeRow 

        match rows with
        | [] -> None
        | _ -> makeTableWithHeadings headings rows |> gridTable |> Some

    let structureNodeChanges (node : StructureNode) : Markdown option = 
        let makeDoc table = 
            let heading = 
                htmlAnchorId (md5Hash node.CommonName)
                             []
                             (text node.Reference ^+^ text node.CommonName)
                    |> h3
            heading ^!!^ table
        node.ValueChanges 
            |> Option.bind changeAttributeTable
            |> Option.map makeDoc
                
                    
    // ************************************************************************
    // Individual change request details

    /// Individual Change Request section header
    let changeRequestSectionHeader (changeRequest : ChangeRequest) : Markdown = 
        let requestId =  changeRequest.Info.ChangeRequestId   

        let title = 
            let refName = sprintf "cr%i" requestId
            let titleText = 
                text "Change request" 
                    ^+^ int64Md requestId
                    ^+^ text changeRequest.StructureChange.RootLabel.Reference
                    ^+^ text changeRequest.StructureChange.RootLabel.ShortName
            htmlAnchorId refName [] titleText

        h2 title
            ^!!^ (text "Asset:" ^+^ text changeRequest.StructureChange.RootLabel.CommonName |> markdownText)
            ^!!^ (text "Request status:" ^+^ text changeRequest.Info.Status |> markdownText)
            ^!!^ markdownText (text "Request time:" ^+^ iso8601DateTimeMd changeRequest.Info.RequestTime)
            ^!!^ markdownText (text "Request type:" ^+^ text changeRequest.Info.RequestType)
            ^!!^ markdownText (text "Comment:" ^+^ text changeRequest.Info.Comment)

    let nodespan (colourName : string) (extraAttrs : HtmlAttrs) (body : Text) : Text = 
        htmlSpan (htmlAttrStyle [backgroundColor colourName] :: extraAttrs) body


    let drawLabel (isRoot : bool) (item : StructureNode)  : Markdown = 
        let makeLabelL (item : AiFlocNode) (wrap : string -> Text) : Text = 
            if isRoot then item.CommonName else item.ShortName
                |> wrap

        let makeLabelR (item : AideFlocNode) (wrap : string -> Text) : Text = 
            if isRoot then item.CommonName else item.ShortName
                |> wrap

        match item with
        | Deleted s -> 
            // no link for deleted
            nodespan deletedRed [] (makeLabelL s text) |> markdownText
        | Common(s1,s2,changes) -> 
            if changes.HasChanges then
                let wrap t1 = inlineLink t1 ("#"+ md5Hash s2.CommonName) None
                nodespan modifiedOrange [] (makeLabelR s2 wrap)  |> markdownText
            else
                makeLabelR s2 text |> markdownText
        | Added(s,changes) -> 
            let wrap t1 = inlineLink t1 ("#"+ md5Hash s.CommonName) None
            nodespan addedGreen [] (makeLabelR s wrap) |> markdownText

    let treeLegend () : Markdown = 
        let specs = 
            let cellSpec1 = { ColumnSpec.Width = 60 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            List.replicate 5 cellSpec1; 
            
        let colourize (colour : string) (source : string) = 
            htmlSpan [htmlAttrStyle [styleDecl "background-color" colour]] (text source)
        let row1 : TableRow = 
            [ markdownText ("Legend:" |> text) 
            ; markdownText ("Added" |> colourize addedGreen)
            ; markdownText ("Deleted" |> colourize deletedRed)
            ; markdownText ("Modified" |> colourize modifiedOrange)
            ; markdownText ("No change" |> text)
            ]
        makeTableWithoutHeadings specs [row1] |> gridTable
        

    let changeRequestTree (hierarchy : Hierarchy<StructureNode>) : Markdown = 
        let tree = 
            hierarchy.ToMarkdownTree() 
                |> RoseTree.mapTree2 (drawLabel true) (drawLabel false)
                |> RoseTree.drawTree
        (text "Structure Changes:" |> markdownText)
            ^!!^ treeLegend ()
            ^!!^ tree


    let changeRequestAttibutes (changeRequest : ChangeRequest) : Markdown = 
        let changes = changeRequest.StructureChange.Flatten()
        vsep (List.map structureNodeChanges changes |> List.choose id)

    let changeRequestSection (changeRequest : ChangeRequest) : Markdown = 
        changeRequestSectionHeader changeRequest
            ^!!^ changeRequestTree changeRequest.StructureChange
            ^!!^ changeRequestAttibutes changeRequest
            ^!!^ linkToTop

    // ************************************************************************
    // Full report


    let makeFullChangeReport (changeScheme : ChangeScheme) : Markdown = 
        h1 (htmlAnchorId "top" [] (text "AIDE Change Scheme"))
            ^!!^ changeSchemeSummaryTable changeScheme
            ^!!^ changeRequestContentsTable changeScheme.ChangeRequests
            ^!!^ vsep (List.map changeRequestSection changeScheme.ChangeRequests)

    let writeFullChangeReport (changeScheme : ChangeScheme) 
                              (pandocOpts : PandocOptions)
                              (outputHtmlFile : string) : Result<unit, string> = 
        let doc = makeFullChangeReport changeScheme
        writeMarkdownReport doc "Aide Change Scheme Report" pandocOpts outputHtmlFile

