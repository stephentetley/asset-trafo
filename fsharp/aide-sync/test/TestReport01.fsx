// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
open System
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.3.2\lib\netstandard2.0"
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

#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20191004\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"
open SLFormat.CommandOptions.CommandOptions

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"
open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc

#load "..\src\AideSync\Base\Common.fs"
#load "..\src\AideSync\Base\Addendum.fs"
#load "..\src\AideSync\AideReport\Internal\Attributes.fs"
#load "..\src\AideSync\AideReport\Internal\Datatypes.fs"
#load "..\src\AideSync\AideReport\Internal\StructureDiff.fs"
#load "..\src\AideSync\AideReport\Internal\BasicQueries.fs"
#load "..\src\AideSync\AideReport\Internal\BuildReport.fs"
#load "..\src\AideSync\AideReport\Internal\PrintReport.fs"
#load "..\src\AideSync\AideReport\Common.fs"
#load "..\src\AideSync\AideReport\AllSchemesReport.fs"
#load "..\src\AideSync\AideReport\ChangeSchemeReport.fs"

open AideSync.Base.Common
open AideSync.Base.Addendum
open AideSync.AideReport.Internal.Attributes
open AideSync.AideReport.Internal.Datatypes
open AideSync.AideReport.Internal.StructureDiff
open AideSync.AideReport.Internal.BasicQueries
open AideSync.AideReport.Internal.BuildReport
open AideSync.AideReport.Internal.PrintReport
open AideSync.AideReport


let activeDb () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\aide_sync_active.sqlite")


let runDb (action : SqliteDb<'a>) : Result<'a, string> = 
    let dbPath = activeDb () 
    let connParams = sqliteConnParamsVersion3 dbPath
    runSqliteDb connParams action


let outputFile (relFileName : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output", relFileName)

// This is the main function ...
// test01 "PCL 81" ;;
// test01 "R131600100" ;;
let test01 (schemeCode : string) : Result<unit, ErrMsg> = 
    let name = sprintf "aide_change_report_%s.html" (safeName schemeCode)
    let htmlOutput = outputFile name

    let config : AideReportConfig = 
        { PathToCss = @"..\..\..\..\..\libs\markdown-css-master\github.css"
          PathToDb = activeDb () }

    runChangeSchemeReport config schemeCode htmlOutput

let test02 () : Result<unit, ErrMsg> = 
    let name = "all_change_schemes.html" 
    let htmlOutput = outputFile name

    let config : AideReportConfig = 
        { PathToCss = @"..\..\..\..\..\libs\markdown-css-master\github.css"
          PathToDb = activeDb () }

    runAllSchemesReport config htmlOutput

/// There was an error building this one - a child node went missing...
/// Now fixed...

let test03 () = 
    findAideDescendants 2022403L |> runDb
    
let test03a () =
    // Currently (4/10/2019) this looses data if there are name 
    // inconsistencies between trees
    let line1 (x : FlocDiff) = 
        sprintf "%s %s" x.Reference x.CommonName

    buildHierarchyDiffs 149397L 557271L 
        |>> Option.map (hierarchyMap line1)
        |>> Option.map (fun x -> x.Flatten())
        |> runDb

let printResult (result : Result<'a, ErrMsg>) : unit = 
    match result with
    | Error msg -> printfn "Error: %s" msg
    | Ok ans -> printfn "Ok: %O" ans

let test04 () = 
    getAllSchemes () |> runDb
