// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Utilities

module PatchReport =

    open System.IO 

    // Possibly SLFormat.CommandOptions should not expose a function called text
    open SLFormat.CommandOptions

    open MarkdownDoc.Markdown
    open MarkdownDoc.Pandoc

    open AssetPatch.Base
    open AssetPatch.Base.Syntax
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


    let patchType (source : PatchType) : Markdown = 
       match source with
       | Download -> "Download" |> rawtext |> doubleAsterisks |> markdownText

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


    let selectionIdType (selId : SelectionId) : Text = 
        match selId with
        | EquiEq _ -> "EQUI EQ" |> text
        | FuncLocEq _ -> "FUNCLOC EQ" |> text

    let selectionIdValue (selId : SelectionId) : Text = 
        match selId with
        | EquiEq x -> x.Number |> text
        | FuncLocEq x -> text x


    let headerTable (source : PatchHeader) : Markdown = 
        let specs = 
            [ { ColumnSpec.Width = 40 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ; { ColumnSpec.Width = 50 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ]
        let makeRow (name : string) (value : Markdown) : TableRow = 
            [ doubleAsterisks (name |> text) |> markdownText ; value ]
        let rows : TableRow list= 
            [ [patchType source.PatchType; nbsp]
            ; makeRow "Data Model:"     (dataModel source.DataModel |> markdownText)
            ; makeRow "Entity Type:"    (entityType source.EntityType |> markdownText)
            ; makeRow "Variant:"        (variant source.Variant)
            ; makeRow "User:"           (text source.User |> markdownText)            
            ; makeRow "Date:"           (source.DateTime.ToString(format="yyyyMMdd") |> cellValue)
            ; makeRow "Time:"           (source.DateTime.ToString(format="HHmmss") |> cellValue)
            ]
        makeTableWithoutHeadings specs rows |> gridTable
        
    let selectionTable (source : SelectionId list) : Markdown = 
        let specs = 
            [ { ColumnSpec.Width = 10 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ; { ColumnSpec.Width = 30 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ; { ColumnSpec.Width = 60 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ]
        let makeRow (ix : int) (selId : SelectionId) : TableRow = 
            [ ix + 1 |> int32Md |> markdownText 
            ; selectionIdType selId |> markdownText 
            ; selectionIdValue selId |> markdownText ]
        let rows : TableRow list = 
            List.mapi makeRow source
        makeTableWithoutHeadings specs rows |> gridTable

    let selectionSection (source : PatchFile<'T>) : Markdown = 
        h2 (text "Selection")
            ^!!^ selectionTable source.Selection

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

    let dataRows (patch : PatchFile<'T>) : Markdown = 
        let makeTable ix (rowAssoc : AssocList<string, string>) = 
            h2 (text "Row" ^+^ int32Md (ix+1))
                ^!!^ dataAssocTable patch.Header.EntityType rowAssoc
                ^!!^ linkToTop
        List.mapi makeTable patch.RowAssocs |> vsep



    let patchToMarkdown (patch : PatchFile<'T>) : Markdown = 
        h1 (text "Patch Report")
            ^!!^ headerTable patch.Header
            ^!!^ selectionSection patch
            ^!!^ dataRows patch
            ^!!^ emptyMarkdown

    let pandocGenHtml (pandocOpts : PandocOptions)
                       (outputDirectory : string)
                       (htmlFileName : string) 
                       (patch : PatchFile<'T>) : Result<unit, string> = 
        let doc = patchToMarkdown patch 
        writeHtml5Markdown "Patch Summary" pandocOpts outputDirectory htmlFileName doc


    
    let patchReport (pathToCssSytlesheet : string) 
                    (outputDirectory : string) 
                    (inputPatch : string)  : Result<unit, string> = 
        let outputHtmlFile = 
            Path.GetFileName(inputPatch) 
                |> fun s -> Path.ChangeExtension(path = s, extension = "html")
        match readPatch inputPatch with
        | Result.Error msg -> Result.Error msg
        | Result.Ok ans -> 
            let opts = pandocHtmlDefaultOptions pathToCssSytlesheet
            pandocGenHtml opts outputDirectory outputHtmlFile ans
            
        