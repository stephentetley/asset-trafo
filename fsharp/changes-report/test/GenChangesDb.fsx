// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
#r "System.Transactions.dll"
open System
open System.IO


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
open SLFormat.CommandOptions.SimpleInvoke

#load "..\src\SLSqlite\Utils.fs"
#load "..\src\SLSqlite\SqliteDb.fs"

open SLSqlite.SqliteDb

#load "CreateDb.fs"
open SLSqlite.CreateDb


#load "..\src\AssetSync\ChangesReport\ImportSchema.fs"
#load "..\src\AssetSync\ChangesReport\PopulateChangesDb.fs"
// open AssetTrafo.Base.Common
open AssetSync.ChangesReport.PopulateChangesDb

let workingDirectory () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output\")

let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output\", relativePath)

let pathToDbTemplate () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\", "change_request.sqlite")

type ErrMsg = string

let main () : Result<unit, ErrMsg> = 
    let assetChangesCsv = @"G:\work\Projects\asset_sync\aide_report\aide_asset_changes_20190809.csv"
    let attributeChangesCsv = @"G:\work\Projects\asset_sync\aide_report\aide_attribute_changes_20190809.csv"

    let dbTemplate = pathToDbTemplate ()
    let dbActive = outputFile "change_requests.sqlite" |> Path.GetFullPath
    printfn "%s" dbActive
    if File.Exists(dbActive) then
        System.IO.File.Delete dbActive
    else ()
    System.IO.File.Copy(sourceFileName = dbTemplate, destFileName = dbActive)

    let connParams = sqliteConnParamsVersion3 dbActive
    runSqliteDb connParams 
        <| sqliteDb { 
                do! insertAsssetChangeRows assetChangesCsv
                do! insertAttributeChangeRows attributeChangesCsv
                return ()
            }

//let temp01 () = 
//    let cwd = workingDirectory ()
//    let ddl = @"D:\coding\work\asset-trafo\fsharp\changes-report\data\change_request_create_db.sql"
//    createDb cwd "TEMP_DB.sqlite" ddl

//let temp02 () = 
//    let cwd = workingDirectory () |> Some
//    runEchoCmd cwd [literal "hello"] (outputFile "output.txt")


//let temp03 () = 
//    let cwd = workingDirectory () |> Some
//    runPipeToEcho cwd "\"hello world from f#\"" (outputFile "output2.txt")

//let temp04 () = 
//    try
//        let cwd = workingDirectory () |> Some
//        runPipeDir cwd |> Some
//    with
//    | expn -> printfn "%s" expn.Message ; None
