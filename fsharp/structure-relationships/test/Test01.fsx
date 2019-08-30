// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
#r "System.Transactions.dll"
open System
open System.IO
open System.Data


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.0.1\lib\netstandard2.0"
#r @"FSharp.Data.dll"
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


#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"
open SLFormat.CommandOptions

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20190830a\lib\netstandard2.0"
#r "MarkdownDoc.dll"
open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc

#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20190823\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core


#load "..\src\AssetSync\Base\Addendum.fs"
#load "..\src\AssetSync\StructureRelationships\Datatypes.fs"
#load "..\src\AssetSync\StructureRelationships\StructureItemDiff.fs"
#load "..\src\AssetSync\StructureRelationships\BasicQueries.fs"
#load "..\src\AssetSync\StructureRelationships\SRDiff.fs"
open AssetSync.StructureRelationships.StructureItemDiff
open AssetSync.StructureRelationships.BasicQueries
open AssetSync.StructureRelationships.SRDiff

let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output\", relativePath)

let dbFile () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\structure_relationships.sqlite")

let getConnParams () : SqliteConnParams = 
    let dbActive = dbFile () |> Path.GetFullPath
    sqliteConnParamsVersion3 dbActive



// e.g test01 2111881L ;;
let test01 (startId : int64) = 
    let connParams = getConnParams ()
    runSqliteDb connParams
        <| sqliteDb { 
                return! findAideDescendants startId
            }

// e.g test02 "SAI00001460" ;;
let test02 (sairef : string) = 
    let connParams = getConnParams ()
    runSqliteDb connParams
        <| sqliteDb { 
                match! findAiAssetIndex sairef with
                | None -> return []
                | Some key -> return! findAiDescendants key
            }

// e.g test03 141913L "SAI00001460" ;;
let test03 (changeReqId : int64) (sairef : string) = 
    let connParams = getConnParams ()
    runSqliteDb connParams
        <| sqliteDb { 
                match! findAideAssetIndex changeReqId sairef with
                | None -> return []
                | Some key -> return! findAideDescendants key
            }


// e.g test04 141913L "SAI00001460" ;;  // This has a couple of diffs (one is a delete and add back)
// or  test04 141013L "SAI00001460" ;;  // This has quite good diffs
// or  test04 148575L "SAI00584748" ;;  // simple additions
let test04 (changeReqId : int64) (sairef : string) = 
    let connParams = getConnParams ()
    let action = 
        sturctureRelationshipsDiff changeReqId sairef
    match runSqliteDb connParams action with
    | Error msg -> printfn "%s" msg
    | Ok diffs -> 
        let tempFile = outputFile "diff_temp.txt"
        diffs 
            |> showDiffs (fun o -> sprintf "%s - %s" o.Reference o.CommonName)
            |> fun x -> File.WriteAllText(path = tempFile, contents = x)




// e.g test05 141913L "SAI00001460" ;;  // Single name change
// or  test05 141013L "SAI00001460" ;;  // Single name change
let test05 (changeReqId : int64) (sairef : string) = 
    let connParams = getConnParams ()
    let action = 
        nameChanges changeReqId sairef
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

// e.g  test06 141913L "SAI00001460" ;;  // This has a couple of diffs (one is a delete and add back, the other a name change)
// or   test06 141013L "SAI00001460" ;;  // This has quite good diffs
// or   test06 148575L "SAI00584748" ;;  // simple additions
//      test06 148574L "SAI00093850" ;;
let test06 (changeReqId : int64) (sairef : string) = 
    let opts = pandocHtmlDefaults @"..\..\..\..\..\libs\markdown-css-master\github.css"
    let connParams = getConnParams ()
    let action = 
        sturctureRelationshipsDiff changeReqId sairef
    match runSqliteDb connParams action with
    | Error msg -> printfn "%s" msg ; Error msg
    | Ok diffs -> 
        let tempFile = outputFile "diff_pcl70_148574.html"
        diffs 
            |> drawStructure |> fun doc -> (h1 (text "Structure Changes") ^!!^ doc )
            |> fun doc -> writeMarkdownReport doc "Structure Changes" opts tempFile
            




