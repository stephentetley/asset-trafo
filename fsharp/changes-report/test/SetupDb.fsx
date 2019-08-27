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


#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20190823\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core



#load "..\src\AssetSync\ChangesReport\ImportSchema.fs"
#load "..\src\AssetSync\ChangesReport\PopulateChangesDb.fs"
open AssetSync.ChangesReport.PopulateChangesDb



let outputDbFile () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\change_requests.sqlite")

let pathToDbTemplate () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\ddl\change_requests.sqlite")

type ErrMsg = string

let main () : Result<unit, ErrMsg> = 
    let changeRequestsCsv = 
        @"G:\work\Projects\asset_sync\aide_report\change_requests_change_reqs_20190827.csv"
    let assetChangesCsv = 
        @"G:\work\Projects\asset_sync\aide_report\change_requests_assets_20190827.csv"
    let attributeChangesCsv = 
        @"G:\work\Projects\asset_sync\aide_report\change_requests_attributes_20190827.csv"
    let repeatedAttributeChangesCsv = 
        @"G:\work\Projects\asset_sync\aide_report\change_requests_repeated_attributes_20190827.csv"
    let workSchemeCsv = 
        @"G:\work\Projects\asset_sync\aide_report\change_requests_schemes_20190827.csv"
    let dbTemplate = pathToDbTemplate ()
    let dbActive = outputDbFile () |> Path.GetFullPath
    printfn "%s" dbActive
    if File.Exists(dbActive) then
        System.IO.File.Delete dbActive
    else ()
    System.IO.File.Copy(sourceFileName = dbTemplate, destFileName = dbActive)

    let connParams = sqliteConnParamsVersion3 dbActive
    runSqliteDb connParams 
        <| sqliteDb { 
                do! insertChangeRequestRows changeRequestsCsv
                do! insertAsssetChangeRows assetChangesCsv
                do! insertAttributeChangeRows attributeChangesCsv
                do! insertRepeatedAttributeChangeRows repeatedAttributeChangesCsv
                do! insertWorkSchemeRows workSchemeCsv
                return ()
            }

