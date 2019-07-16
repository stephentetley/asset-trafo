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

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20190716\lib\netstandard2.0"
#r "MarkdownDoc.dll"
open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc


#load "..\src\AssetTrafo\Base\Common.fs"
#load "..\src\AssetTrafo\AideReport\Syntax.fs"
#load "..\src\AssetTrafo\AideReport\ReadCsv.fs"
open AssetTrafo.Base.Common
open AssetTrafo.AideReport.Syntax
open AssetTrafo.AideReport.ReadCsv

let outputDirectory () = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", "output")

let getOutputFile (relFileName : string) = 
    let dir = outputDirectory () in Path.Combine(dir, relFileName)


let tempDatetime () : System.DateTime = 
    DateTime.Now

let alignLeft (width : int) : ColumnSpec = 
    { Width = width; Alignment = Alignment.AlignLeft }
    

let test01 () = 
    let rows = 
        readAttributeChangeExport @"G:\work\Projects\asset_sync\aide_report\attribute_changes1.csv"
            |> Seq.map convertAttributeChangeRow
            |> Seq.toList
    let doc = makeReport rows
    let mdPath = getOutputFile "changes_report.md"
    doc.Save(columnWidth = 140, outputPath = mdPath)
    runPandocHtml (outputDirectory ()) "changes_report.md" "changes_report.html" (Some "Changes Report") pandocDefaults



