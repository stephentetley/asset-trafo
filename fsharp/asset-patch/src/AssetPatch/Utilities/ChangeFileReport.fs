// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Utilities

module ChangeFileReport =

    open System.IO 

    // Possibly SLFormat.CommandOptions should not expose a function called text
    open SLFormat.CommandOptions

    open MarkdownDoc.Markdown
    open MarkdownDoc.Pandoc

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.Acronyms
    open AssetPatch.Base.Parser


    // ************************************************************************
    // Invoking Pandoc

    let pandocHtmlDefaultOptions (pathToCss : string) : PandocOptions = 
        let highlightStyle = argument "--highlight-style" &= argValue "tango"
        let selfContained = argument "--self-contained"
        /// Github style is nicer for tables than Tufte
        /// Note - body width has been changed on both stylesheets
        let css = argument "--css" &= doubleQuote pathToCss
        { Standalone = true
          InputExtensions = []
          OutputExtensions = []
          OtherOptions = [ css; highlightStyle; selfContained ]  }


    let writeHtml5Markdown (pageTitle : string)
                            (pandocOpts : PandocOptions)
                            (outputDirectory : string) 
                            (htmlFileName : string) 
                            (report: Markdown) : Result<unit, string> = 
        let mdFileName = Path.ChangeExtension(htmlFileName, "md")       
        let mdFileAbsPath =
            Path.Combine(outputDirectory, mdFileName)
        /// Need a pretty wide page width...
        writeMarkdown 260 report mdFileAbsPath
        let retCode = 
            runPandocHtml5 
                true 
                outputDirectory 
                mdFileName
                htmlFileName
                (Some pageTitle)
                pandocOpts
        match retCode with
        | Result.Ok i -> printfn "Return code: %i" i ; Result.Ok ()
        | Result.Error msg -> Result.Error msg


    let cellValue (str : string) : Markdown = 
        match str with
        | null | "" -> nbsp 
        | _ -> str |> text |> markdownText

    let linkToTop : Markdown = 
        inlineLink "Back to top" "#top" None |> markdownText


    let fileType (source : FileType) : Markdown = 
        let value = 
            match source with
            | Download -> "Download" 
            | Upload -> "Upload"
        value |> rawtext |> doubleAsterisks |> markdownText

    let dataModel (source : DataModel) : Text = 
        match source with
        | U1 -> "U1" |> rawtext

    let entityType (source : EntityType) : Text = 
        let name = 
            match source with
            | FuncLoc -> "FUNCLOC"
            | ClassFloc -> "CLASSFLOC"
            | ValuaFloc -> "VALUAFLOC"
            | Equi -> "EQUI"
            | ClassEqui -> "CLASSEQUI" 
            | ValuaEqui -> "VALUAEQUI"
        name |> rawtext
    
    let variant () : Markdown = 
        nbsp



    let headerTable (source : FileHeader) : Markdown = 
        let specs = 
            [ { ColumnSpec.Width = 40 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ; { ColumnSpec.Width = 50 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ]
        let makeRow (name : string) (value : Markdown) : TableRow = 
            [ doubleAsterisks (name |> text) |> markdownText ; value ]
        let rows : TableRow list= 
            [ [fileType source.FileType; nbsp]
            ; makeRow "Data Model:"     (dataModel source.DataModel |> markdownText)
            ; makeRow "Entity Type:"    (entityType source.EntityType |> markdownText)
            ; makeRow "Variant:"        (variant source.Variant)
            ; makeRow "User:"           (text source.User |> markdownText)            
            ; makeRow "Date:"           (source.DateTime.ToString(format="yyyyMMdd") |> cellValue)
            ; makeRow "Time:"           (source.DateTime.ToString(format="HHmmss") |> cellValue)
            ]
        makeTableWithoutHeadings specs rows |> gridTable
        


    let dataAssocTable (entityType : EntityType) 
                       (source : AssocList<string, string>) : Markdown = 
        let title = text >> doubleAsterisks >> markdownText
        let headings =
            [ alignLeft 10 (title "Index")
            ; alignLeft 30 (title "Field")
            ; alignLeft 50 (title "Description")
            ; alignLeft 40 (title "Value")
            ]
        let makeRow (ix :int) (name, value) : TableRow = 
            [ ix + 1 |> int32Md |> markdownText 
            ; name |> cellValue
            ; decodeAcronym entityType name 
                |> Option.defaultValue "" |> text |> markdownText 
            ; value |> text |> markdownText 
            ]
        let rows : TableRow list = 
            List.mapi makeRow (AssocList.toList source)
        makeTableWithHeadings headings rows |> gridTable

    let dataRows (patch : ChangeFile) : Markdown = 
        let makeTable ix (rowAssoc : AssocList<string, string>) = 
            h2 (text "Row" ^+^ int32Md (ix+1))
                ^!!^ dataAssocTable patch.Header.EntityType rowAssoc
                ^!!^ linkToTop
        List.mapi makeTable patch.RowAssocs |> vsep



    let patchToMarkdown (patch : ChangeFile) : Markdown = 
        h1 (text "Patch Report")
            ^!!^ headerTable patch.Header
            ^!!^ dataRows patch
            ^!!^ emptyMarkdown

    let pandocGenHtml (pandocOpts : PandocOptions)
                       (outputDirectory : string)
                       (htmlFileName : string) 
                       (patch : ChangeFile) : Result<unit, string> = 
        let doc = patchToMarkdown patch 
        let title = sprintf "Patch Summary (%s)" (patch.Header.EntityType.ToString())
        writeHtml5Markdown title pandocOpts outputDirectory htmlFileName doc


    
    let changeFileReport (pathToCssSytlesheet : string) 
                         (outputDirectory : string) 
                         (inputPatch : string)  : Result<unit, string> = 
        let outputHtmlFile = 
            Path.GetFileName(inputPatch) 
                |> fun s -> Path.ChangeExtension(path = s, extension = "html")
        match readChangeFile inputPatch with
        | Result.Error msg -> Result.Error msg
        | Result.Ok ans -> 
            let opts = pandocHtmlDefaultOptions pathToCssSytlesheet
            pandocGenHtml opts outputDirectory outputHtmlFile ans
            
        