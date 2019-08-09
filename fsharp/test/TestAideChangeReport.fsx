﻿// Copyright (c) Stephen Tetley 2019
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
#load "..\src\AssetTrafo\AideChangeReport\Syntax.fs"
#load "..\src\AssetTrafo\AideChangeReport\ReadCsv.fs"
#load "..\src\AssetTrafo\AideChangeReport\ChangeReport.fs"
open AssetTrafo.AideChangeReport.Syntax
open AssetTrafo.AideChangeReport.ReadCsv
open AssetTrafo.AideChangeReport.ChangeReport


let getOutputFile (relFileName : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\output", relFileName)




let pandocHtmlOptions () : PandocOptions = 
    pandocHtmlDefaults @"..\..\..\libs\markdown-css-master\github.css"





let test01 () : Result<unit, string> = 
    generateChangesReport
        { AssetChangesCsv = Some @"G:\work\Projects\asset_sync\aide_report\aide_asset_changes_20190809.csv"
          AttributeChangesCsv = Some @"G:\work\Projects\asset_sync\aide_report\aide_attribute_changes_20190809.csv" }
        (pandocHtmlOptions ())
        (getOutputFile "changes_report_20190809.html")

// Note - look at Microsoft.FSharp.Reflection ...