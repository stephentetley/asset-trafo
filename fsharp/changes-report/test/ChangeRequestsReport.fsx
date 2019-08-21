// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
open System
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.1.1\lib\netstandard2.0"
#r "FSharp.Data.dll"
open FSharp.Data

#I @"C:\Users\stephen\.nuget\packages\System.Data.SQLite.Core\1.0.111\lib\netstandard2.0"
#r "System.Data.SQLite.dll"
open System.Data.SQLite

// A hack to get over Dll loading error due to the native dll `SQLite.Interop.dll`
[<Literal>] 
let SQLiteInterop = @"C:\Users\stephen\.nuget\packages\System.Data.SQLite.Core\1.0.111\runtimes\win-x64\native\netstandard2.0"

Environment.SetEnvironmentVariable("PATH", 
    Environment.GetEnvironmentVariable("PATH") + ";" + SQLiteInterop
    )

#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20190820\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"
open SLFormat.CommandOptions.CommandOptions

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20190821a\lib\netstandard2.0"
#r "MarkdownDoc.dll"
open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc

#load "..\src\AssetSync\ChangesReport\Addendum.fs"
#load "..\src\AssetSync\ChangesReport\Datatypes.fs"
#load "..\src\AssetSync\ChangesReport\BuildReport.fs"
#load "..\src\AssetSync\ChangesReport\PrintReport.fs"
open AssetSync.ChangesReport.Addendum
open AssetSync.ChangesReport.Datatypes
open AssetSync.ChangesReport.BuildReport
open AssetSync.ChangesReport.PrintReport

let outputFile (relFileName : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output", relFileName)


let test01 (chreqId : int64) = 
    let dbActive = outputFile "change_requests.sqlite" |> Path.GetFullPath
    let connParams = sqliteConnParamsVersion3 dbActive

    runSqliteDb connParams 
        <| sqliteDb { 
                let! info = getChangeRequestInfo chreqId
                let! xs = getAssetChanges chreqId
                let! ys = getAttributeChanges chreqId
                return (info, xs, ys)
            }
