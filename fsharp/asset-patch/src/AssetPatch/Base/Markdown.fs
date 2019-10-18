// Copyright (c) Stephen Tetley 2019
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


    let patchType (source : PatchType) : Markdown = 
       match source with
       | Download -> "Download" |> rawtext |> h1

    let patchToMarkdown (patch : PatchFile) : Markdown = 
        patchType patch.PatchType

    let pandocGenHtml (pandocOpts : PandocOptions)
                       (outputHtmlFile : string) 
                       (patch : PatchFile) : Result<unit, string> = 
        let doc = patchToMarkdown patch 
        writeHtml5Markdown "Patch Summary" pandocOpts outputHtmlFile doc

        