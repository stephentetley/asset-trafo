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

#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20190823\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"
open SLFormat.CommandOptions.CommandOptions

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20190903\lib\netstandard2.0"
#r "MarkdownDoc.dll"
open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc

#load "..\src\AideSync\Base\Addendum.fs"
#load "..\src\AideSync\Datatypes.fs"
#load "..\src\AideSync\BasicQueries.fs"
#load "..\src\AideSync\DiffImplementation.fs"
#load "..\src\AideSync\Metrics.fs"
#load "..\src\AideSync\BuildReport.fs"
#load "..\src\AideSync\BuildSRDiff.fs"
#load "..\src\AideSync\PrintReport.fs"
open AideSync.Datatypes
open AideSync.BasicQueries
open AideSync.DiffImplementation
open AideSync.BuildReport
open AideSync.BuildSRDiff
open AideSync.PrintReport

let outputFile (relFileName : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output", relFileName)

let pathToDb () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\aide_sync_active.sqlite")

let getConnParams () : SqliteConnParams = 
    let dbActive = pathToDb () |> Path.GetFullPath
    sqliteConnParamsVersion3 dbActive


let pandocHtmlOptions () : PandocOptions = 
    pandocHtmlDefaults @"..\..\..\..\..\libs\markdown-css-master\github.css"

let runChangeRequestsReport (chreqIds : int64 list) 
                            (outputHtmlFile : string) : Result<unit, ErrMsg> = 
    let dbActive = pathToDb () |> Path.GetFullPath
    let connParams = sqliteConnParamsVersion3 dbActive
    let pandocOpts = pandocHtmlOptions ()

    match runSqliteDb connParams (mapM buildChangeRequest chreqIds) with
    | Error msg -> printfn "Fail: %s" msg ; Error "Bad"
    | Ok ochanges -> 
        let changes = List.choose id ochanges
        writeChangeRequestsReport changes pandocOpts outputHtmlFile


let test01 () =
    let changeRequests = [ 15742L; 148364L; 148365L; 148366L; 148367L; 148372L; 148374L ]
    // let changeRequests = [148574L]
    let htmlOutput = outputFile "change_request_report_20190827.html"
    runChangeRequestsReport changeRequests htmlOutput


let runChangeSchemeReport (schemeCode : string) 
                            (outputHtmlFile : string) : Result<unit, ErrMsg> = 
    let connParams = getConnParams ()
    let pandocOpts = pandocHtmlOptions ()

    match runSqliteDb connParams (buildChangeScheme schemeCode ) with
    | Error msg -> printfn "Fail: %s" msg ; Error "Bad"
    | Ok None -> Error (sprintf "Could not find scheme matching '%s'" schemeCode)
    | Ok (Some scheme) -> 
        writeFullReport scheme pandocOpts outputHtmlFile


// test02 "PCL 70" ;;   // Structure changes
let test02 (changeScheme : string) =
    let htmlOutput = outputFile "change_scheme_report_20190828.html"
    runChangeSchemeReport changeScheme htmlOutput


// e.g test03 2111881L ;;
let test03 (startId : int64) = 
    let connParams = getConnParams ()
    runSqliteDb connParams
        <| findAideDescendants startId
           
    
// e.g test04 "SAI00001460" ;;
let test04 (sairef : string) = 
    let connParams = getConnParams ()
    runSqliteDb connParams
        <| sqliteDb { 
                match! findAiAssetId sairef with
                | None -> return []
                | Some uid -> return! findAiDescendants uid
            }


// e.g test05 141913L "SAI00001460" ;;
let test05 (changeReqId : int64) (sairef : string) = 
    let connParams = getConnParams ()
    runSqliteDb connParams
        <| sqliteDb { 
                match! findAiAssetId sairef with
                | None -> return []
                | Some uid1 -> 
                    match! findAideAssetId changeReqId uid1 with
                    | None -> return []
                    | Some uid2 -> return! findAideDescendants uid2
            }


// e.g test06 141913L "SAI00001460" ;;  // This has a couple of diffs (one is a delete and add back)
// or  test06 141013L "SAI00001460" ;;  // This has quite good diffs
// or  test06 148575L "SAI00584748" ;;  // simple additions
let test06 (changeReqId : int64) (sairef : string) = 
    let connParams = getConnParams ()
    let action : SqliteDb<Differences> = 
        sqliteDb { 
            match! findAiAssetId sairef with 
            | Some sairef -> return! getDifferences changeReqId sairef
            | None -> return! throwError "None"
        }
    match runSqliteDb connParams action with
    | Error msg -> printfn "%s" msg
    | Ok diffs -> 
        let tempFile = outputFile "diff_temp.txt"
        diffs 
            |> showDiffs (fun o -> sprintf "%s - %s" o.Reference o.CommonName)
                            (fun o -> sprintf "%s - %s" o.Reference o.CommonName)
            |> fun x -> File.WriteAllText(path = tempFile, contents = x)




// e.g test07 141913L "SAI00001460" ;;  // Single name change
let test07 (changeReqId : int64) (sairef : string) = 
    let connParams = getConnParams ()
    let action = 
        sqliteDb { 
            match! findAiAssetId sairef with 
            | Some sairef -> return! getDifferences changeReqId sairef
            | None -> return! throwError "None"
        }
    match runSqliteDb connParams action with
    | Error msg -> printfn "%s" msg
    | Ok [] -> printfn "No changes identified"
    | Ok changes -> 
        List.iter (printfn "%O") changes


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
    writeMarkdown 360 doc mdFileAbsPath
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

// e.g  test08 141913L "SAI00001460" ;;  // This has a couple of diffs (one is a delete and add back, the other a name change)
// or   test08 141013L "SAI00001460" ;;  // This has quite good diffs
// or   test08 148575L "SAI00584748" ;;  // simple additions
//      test08 148574L "SAI00093850" ;;  // structure changes

let test08 (changeReqId : int64) (sairef : string) = 
    let opts = pandocHtmlDefaults @"..\..\..\..\..\libs\markdown-css-master\github.css"
    let connParams = getConnParams ()
    let action = 
        sqliteDb { 
            match! findAiAssetId sairef with 
            | Some sairef -> return! getDifferences changeReqId sairef
            | None -> return! throwError "None"
        }
    match runSqliteDb connParams action with
    | Error msg -> printfn "%s" msg ; Error msg
    | Ok diffs -> 
        let tempFile = outputFile "diff_pcl70_148574.html"
        diffs 
            |> drawStructure |> fun doc -> (h1 (text "Structure Changes") ^!!^ doc )
            |> fun doc -> writeMarkdownReport doc "Structure Changes" opts tempFile