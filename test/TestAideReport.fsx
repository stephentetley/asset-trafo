// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
open System
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.1.1\lib\netstandard2.0"
#r "FSharp.Data.dll"
open FSharp.Data


#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190712\lib\netstandard2.0"
#r "SLFormat.dll"
open SLFormat.CommandOptions.CommandOptions

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20190716d\lib\netstandard2.0"
#r "MarkdownDoc.dll"
open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc


#load "..\src\AssetTrafo\Base\Common.fs"
#load "..\src\AssetTrafo\AideReport\Syntax.fs"
#load "..\src\AssetTrafo\AideReport\ReadCsv.fs"
#load "..\src\AssetTrafo\AideReport\MarkdownReport.fs"
open AssetTrafo.AideReport.ReadCsv
open AssetTrafo.AideReport.MarkDownReport


let outputDirectory () = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", "output")

let getOutputFile (relFileName : string) = 
    let dir = outputDirectory () in Path.Combine(dir, relFileName)


let tempDatetime () : System.DateTime = 
    DateTime.Now

let alignLeft (width : int) : ColumnSpec = 
    { Width = width; Alignment = Alignment.AlignLeft }
    


let pandocHtmlOptions () : PandocOptions = 
    let highlightStyle = argument "--highlight-style" &= argValue "tango"
    let selfContained = argument "--self-contained"
    /// Github style is nicer for tables than Tufte
    let css = 
        argument "--css" &= doubleQuote @"..\..\..\libs\markdown-css-master\github.css"
    { Standalone = true
      InputExtensions = []
      OutputExtensions = []
      OtherOptions = [ css; highlightStyle; selfContained ]  }


let test01 () = 
    let assetRows = 
        readAssetChangeExport @"G:\work\Projects\asset_sync\aide_report\asset_change_request1.csv"
            |> Seq.map convertAssetChangeRow
            |> Seq.toList

    let attrRows = 
        readAttributeChangeExport @"G:\work\Projects\asset_sync\aide_report\aide_attribute_change_request1.csv"
            |> Seq.map convertAttributeChangeRow
            |> Seq.toList

    let doc = makeReport assetRows attrRows
    let mdPath = getOutputFile "changes_report.md"
    doc.Save(columnWidth = 140, outputPath = mdPath)
    runPandocHtml5 true 
                    (outputDirectory ()) 
                    "changes_report.md"
                    "changes_report.html" 
                    (Some "Changes makeReport")
                    (pandocHtmlOptions ())


