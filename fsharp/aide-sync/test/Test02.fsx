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

#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20191001\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"
open SLFormat.CommandOptions.CommandOptions

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20190903\lib\netstandard2.0"
#r "MarkdownDoc.dll"
open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc

#load "..\src\AideSync\Base\Common.fs"
#load "..\src\AideSync\Base\Addendum.fs"
#load "..\src\AideSync\AideReport\Attributes.fs"
#load "..\src\AideSync\AideReport\Datatypes.fs"
#load "..\src\AideSync\AideReport\StructureDiff.fs"
#load "..\src\AideSync\AideReport\BasicQueries.fs"
#load "..\src\AideSync\AideReport\BuildReport.fs"

open AideSync.Base.Common
open AideSync.Base.Addendum
open AideSync.AideReport.Attributes
open AideSync.AideReport.Datatypes
open AideSync.AideReport.StructureDiff
open AideSync.AideReport.BasicQueries
open AideSync.AideReport.BuildReport

let runDb (action : SqliteDb<'a>) : Result<'a, string> = 
    let dbActive = 
        Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\aide_sync_active.sqlite")

    let connParams = sqliteConnParamsVersion3 dbActive
    runSqliteDb connParams action

let test01 () = 
    let kidAction schemeId = 
        sqliteDb { 
            let! changeInfo = getChangeRequestInfo schemeId
            let! rootIds = getChangeRequestAIRootIds changeInfo.ChangeRequestId
            return (changeInfo, rootIds)
        }

    let action = 
        sqliteDb { 
            let! info = getChangeSchemeInfo "R131600100"
            let! schemeIds = getSchemeChangeRequestIds info.SchemeCode
            let! changeInfos = smapM kidAction schemeIds
            return (info,changeInfos)
        }
    action |> runDb


let test02 () = 
    findAiDescendants 523007L |> runDb


// This was veeerrrryyy slooowww - much improved by adding indices to the db
let test03 () = 
    let action = 
        sqliteDb {
            let! aideId = findAideAssetId 149392L 523007L |> getOptional
            let! flatTree = findAideDescendants aideId
            return flatTree
        }
    action |> runDb

let test04 () = 
    let action = 
        buildHierarchyDiffs 149392L 523007L
    runDb action

let test05 () = 
    getAssetPropertyChanges 6059L |> runDb


let test06 () = 
    getAssetAttributeChanges 2122243L |> runDb

let test07 () = 
    getAssetRepeatedAttributeChanges 28888L |> runDb


let test08 () = 
    let action = 
        sqliteDb { 
            match! buildHierarchyDiffs 149392L 523007L with
            | Some tree -> return! expandFlocDiffHierarchy tree |>> pruneTree
            | None -> return None
        }
    runDb action


let test09 () = 
    getChangeScheme "R131600100" |> runDb
    