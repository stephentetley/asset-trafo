// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module PrintReport =
    
    open System.IO

    // Note there is a name clash, so open this before MarkdownDoc.Markdown
    // Possibly SLFormat.CommandOptions should not expose a function called text
    open SLFormat.CommandOptions

    open MarkdownDoc.Markdown
    open MarkdownDoc.Markdown.InlineHtml
    open MarkdownDoc.Markdown.CssColors
    open MarkdownDoc.Pandoc
    

    open AideSync.Base.Addendum
    open AideSync.Datatypes
    open AideSync.DiffImplementation
    open AideSync.Metrics


    let headingTitle : string -> Markdown = 
        markdownText << doubleAsterisks << text

    let linkToTop : Markdown = 
        inlineLink "Back to top" "#top" None |> markdownText



    let span (colour : ColorName) (body : Text) : Markdown = 
        htmlSpan [ attrStyle [backgroundColor colour] ] body |> markdownText

    let valueChanges (aiValue :string) (aideValue : string) : Markdown * Markdown = 
        let maketext str = str |> text |> markdownText
        match aiValue, aideValue with 
        | "", s2 -> emptyMarkdown, span paleGreen (text s2) 
        | s1, "" -> span lightCoral (text s1), emptyMarkdown
        | s1, s2 -> text s1 |> strikeout |> markdownText, span gold (text s2) 


    // ************************************************************************
    // Change scheme

    /// Change Scheme Table 
    let changeSchemeSummaryTable (scheme : ChangeScheme) : Markdown =
        let specs = 
            [ { ColumnSpec.Width = 20 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ; { ColumnSpec.Width = 40 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ]
        let makeRow (name : string) (value : Text) : TableRow = 
            [ doubleAsterisks (name |> text) |> markdownText ; value |> markdownText ]
        let rows : TableRow list= 
            [ makeRow "Scheme Code"         (text scheme.Info.Code)
            ; makeRow "Scheme Id"           (int64Md scheme.Info.SchemeId)
            ; makeRow "Scheme Name"         (text scheme.Info.Name)
            ; makeRow "Solution Provider"   (text scheme.Info.SolutionProvider)
            ; makeRow "No. of Change Requests" (numberOfChangeRequests scheme |> int32Md)
            ; makeRow "Total Property Changes" (numberOfPropertyChanges scheme |> int32Md)
            ; makeRow "Total Attribute Changes (inluding repeated attributes)" (numberOfAttributeChangesAll scheme |> int32Md)
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
            htmlAnchorId refname (text "Change request" ^+^ int64Md requestId)

        h2 title
            ^!!^ markdownText (text "Request status:" ^+^ text changeRequestInfo.Status)
            ^!!^ markdownText (text "Request time:" ^+^ iso8601DateTimeMd changeRequestInfo.RequestTime)
            ^!!^ markdownText (text "Request type:" ^+^ text changeRequestInfo.RequestType)
            ^!!^ markdownText (text "Comment:" ^+^ text changeRequestInfo.Comment)


    // ************************************************************************
    // Attribute changes

    //let attributeValue (attrValue : AttributeValue) : Text = 
    //    text attrValue.Value
   
    

    let attributeDeltaRow (assetInfo : AssetInfo) 
                          (attrDelta : AttributeDelta) : Table.TableRow = 
        let aiValue,aideValue = 
            valueChanges attrDelta.AiValue.Value attrDelta.AideValue.Value
        [ assetInfo.AssetReference      |> text     |> markdownText
        ; assetInfo.AssetName           |> text     |> markdownText
        ; attrDelta.AttributeName       |> text     |> markdownText 
        ; aiValue
        ; aideValue
        ]

    let attributeChangesTable (assetInfo : AssetInfo) 
                              (attrChanges : AttributeDelta list) : Markdown option = 
        let headings =
            [ alignLeft 30 (headingTitle "Asset Reference")
            ; alignLeft 40 (headingTitle "Asset Name")
            ; alignLeft 30 (headingTitle "Attribute Name")
            ; alignLeft 50 (headingTitle "AI Value")
            ; alignLeft 50 (headingTitle "AIDE Value")
            ]
        let (rows : TableRow list) = 
            attrChanges |> List.map (attributeDeltaRow assetInfo)
        match rows with
        | [] -> None
        | _ -> makeTableWithHeadings headings rows |> gridTable |> Some
    
    let attributeChangesSection (assetInfo : AssetInfo) (attributeChanges : AttributeDelta list) : Markdown = 
        match attributeChangesTable assetInfo attributeChanges with
        | None -> asterisks (text "No attribute changes")  |> h3
        | Some table -> 
            h3 (text "Attribute Changes" )
                ^!!^ table

    // ************************************************************************
    // Attribute changes

   
    let repeatedAttributeChangeRow (assetInfo : AssetInfo) 
                                    (repAttrChange : RepeatedAttributeDelta) : Table.TableRow = 
        let aiValue,aideValue = 
            valueChanges repAttrChange.AiValue.Value repAttrChange.AideValue.Value
        [ assetInfo.AssetReference              |> text     |> markdownText
        ; assetInfo.AssetName                   |> text     |> markdownText
        ; repAttrChange.AttributeSetName        |> text     |> markdownText 
        ; repAttrChange.RepeatedAttributeName   |> text     |> markdownText 
        ; aiValue 
        ; aideValue
        ]

    let repeatedAttributeChangesTable (assetInfo : AssetInfo) 
                (repAttrChanges : RepeatedAttributeDelta list) : Markdown option = 
        let headings =
            [ alignLeft 30 (headingTitle "Asset Reference")
            ; alignLeft 40 (headingTitle "Asset Name")
            ; alignLeft 40 (headingTitle "Attribute Set Name")
            ; alignLeft 40 (headingTitle "Attribute Name")
            ; alignLeft 30 (headingTitle "AI Value")
            ; alignLeft 30 (headingTitle "AIDE Value")
            ]
        let (rows : TableRow list) = 
            repAttrChanges |> List.map (repeatedAttributeChangeRow assetInfo)
        match rows with
        | [] -> None
        | _ -> makeTableWithHeadings headings rows |> gridTable |> Some
    
    let repeatedAttributeChangesSection (assetInfo : AssetInfo) 
                (repeatedAttributeChanges : RepeatedAttributeDelta list) : Markdown = 
        match repeatedAttributeChangesTable assetInfo repeatedAttributeChanges with
        | None -> asterisks (text "No repeated attribute changes")  |> h3
        | Some table -> 
            h3 (text "Repeated Attribute Changes" )
                ^!!^ table

    // ************************************************************************
    // Asset "property" changes


    


    let assetPropertyChangeRow (assetInfo : AssetInfo)
                                (assetProperty : AssetPropertyDelta) : Table.TableRow = 
        let aiValue,aideValue = valueChanges assetProperty.AiValue assetProperty.AideValue
        [ assetInfo.AssetReference      |> text |> markdownText
        ; assetInfo.AssetName           |> text |> markdownText
        ; assetProperty.PropertyName    |> text |> markdownText 
        ; aiValue
        ; aideValue
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
            assetChange.AssetChanges.AssetProperties
                |> List.map (assetPropertyChangeRow assetChange.AssetInfo)

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
    // Structure changes
    
    let structureItemDiff (diff : StructureItemDiff) : Markdown = 
        match diff with
        | InLeft s -> text "Delete:" ^+^ text s.Reference |> markdownText
        | InRight s -> text "Add:" ^+^ text s.Reference |> markdownText
        | Match s -> text "Id:" ^+^ text s.Reference |> markdownText
        | Difference(s1,s2) -> text "Diff:" ^+^ text s1.Reference |> markdownText

    let positiveDifferences (diffs : StructureItemDiff list) : Markdown = 
        let select1 (sd : StructureItemDiff) : StructureItemDiff option = 
            match sd with
            | Match _ -> None
            | _ -> Some sd
        diffs 
            |> List.choose select1
            |> List.map structureItemDiff
            |> vsep

    let structureChange (structChange : AssetStructureChange) : Markdown = 
        let headline = 
            text structChange.AssetInfo.AssetReference ^+^ text structChange.AssetInfo.CommonName
                |> doubleAsterisks 
                |> markdownText
        let summary = 
                (numberOfDifferences structChange.StructureChanges |> int32Md)
                    ^+^ text "Structure changes"
                |> markdownText
        headline
            ^!!^ summary
            ^!!^ positiveDifferences structChange.StructureChanges
            ^!!^ drawPrunedStructure structChange.StructureChanges


    let structureChangesSection (structureChanges : AssetStructureChange list) : Markdown = 
        vcat (List.map structureChange structureChanges)

    // ************************************************************************
    // Build the document

    let changeRequestBody (changeRequest : ChangeRequest) : Markdown = 
        match changeRequest with
        | UnhandledChangeRequest info -> 
            let t1 = text "Unhandled change request type" 
                        ^+^ doubleAsterisks (text info.RequestType)
            markdownText t1
        | AideChange(_, structureChanges) -> 
            structureChangesSection structureChanges

        | AttributeChange(info, changes) -> 
            assetPropertyChangesSection changes
            //^!!^ attributeChangesSection info changes
            //^!!^ repeatedAttributeChangesSection zs


    let makeChangeRequest1 (changeRequest : ChangeRequest) : Markdown = 
        changeRequestSectionHeader (changeRequest.Info)
            ^!!^ changeRequestBody changeRequest
            ^!!^ linkToTop

    let makeChangeRequestsReport (changeRequests : ChangeRequest list) : Markdown = 
        let requestInfos = changeRequests |> List.map (fun x -> x.Info)

        h1 (htmlAnchorId "top" (text "AIDE Change Requests"))
            ^!!^ changeRequestInfosSection requestInfos
            ^!!^ vsep (List.map makeChangeRequest1 changeRequests)
      
    let makeChangeSchemeReport (changeScheme : ChangeScheme) : Markdown = 
        let requestInfos = changeScheme.ChangeRequests |> List.map (fun x -> x.Info)

        h1 (htmlAnchorId "top" (text "AIDE Change Scheme"))
            ^!!^ changeSchemeSummaryTable changeScheme
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