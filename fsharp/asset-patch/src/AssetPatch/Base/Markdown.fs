﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Markdown =

    open System.IO 

    // Possibly SLFormat.CommandOptions should not expose a function called text
    open SLFormat.CommandOptions

    open MarkdownDoc.Markdown
    open MarkdownDoc.Pandoc

    open AssetPatch.Base.Syntax


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
                            (outputHtmlFile : string) 
                            (report: Markdown) : Result<unit, string> = 
        
        let mdFileAbsPath = Path.ChangeExtension(outputHtmlFile, "md") 
        let mdFileName = Path.GetFileName(mdFileAbsPath)
        let htmlFileName = Path.GetFileName(outputHtmlFile)
        let outputDirectory = Path.GetDirectoryName(outputHtmlFile)
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


    let headerTable (source : PatchFile) : Markdown = 
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
            ; makeRow "Time:"           (source.DateTime.ToString(format="hhmmss") |> cellValue)
            ]
        makeTableWithoutHeadings specs rows |> gridTable
        
    let selectionTable (source : SelectionId list) : Markdown = 
        let specs = 
            [ { ColumnSpec.Width = 30 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ; { ColumnSpec.Width = 60 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ]
        let makeRow (selId : SelectionId) : TableRow = 
            [ selectionIdType selId |> markdownText 
            ; selectionIdValue selId |> markdownText ]
        let rows : TableRow list = 
            List.map makeRow source
        makeTableWithoutHeadings specs rows |> gridTable

    let selectionSection (source : PatchFile) : Markdown = 
        h2 (text "Selection")
            ^!!^ selectionTable source.Selection

    let dataAssocTable (source : (string * string) list) : Markdown = 
        let specs = 
            [ { ColumnSpec.Width = 30 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ; { ColumnSpec.Width = 40 ; ColumnSpec.Alignment = Alignment.AlignLeft }
            ]
        let makeRow (name, value) : TableRow = 
            [ name |> cellValue; value |> text |> markdownText ]
        let rows : TableRow list = 
            List.map makeRow source
        makeTableWithoutHeadings specs rows |> gridTable

    let dataRows (patch : PatchFile) : Markdown = 
        let makeTable ix rowAssoc = 
            h2 (text "Row" ^^ int32Md (ix+1))
                ^!!^ dataAssocTable rowAssoc
                ^!!^ linkToTop
        List.mapi makeTable patch.RowAssocs |> vsep

    let patchToMarkdown (patch : PatchFile) : Markdown = 
        h1 (text "Patch Report")
            ^!!^ headerTable patch
            ^!!^ selectionSection patch
            ^!!^ dataRows patch
            ^!!^ emptyMarkdown

    let pandocGenHtml (pandocOpts : PandocOptions)
                       (outputHtmlFile : string) 
                       (patch : PatchFile) : Result<unit, string> = 
        let doc = patchToMarkdown patch 
        writeHtml5Markdown "Patch Summary" pandocOpts outputHtmlFile doc

        