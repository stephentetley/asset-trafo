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

#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20190818\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.SqliteDb

#load "..\src\AssetSync\ChangesReport\Addendum.fs"
#load "..\src\AssetSync\ChangesReport\StructRelationshipsDb.fs"
open AssetSync.ChangesReport.StructRelationshipsDb

let workingDirectory () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output\")

let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output\", relativePath)

let pathToDbTemplate () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\", "structure_relationships.sqlite")


type ErrMsg = string

let main () : Result<unit, ErrMsg> = 
    let aideStructRelationshipsCsv = @"G:\work\Projects\asset_sync\aide_report\aide_structure_relationships_20190816.csv"
    let aideAssetLookupsCsv = @"G:\work\Projects\asset_sync\aide_report\aide_asset_lookups_20190816.csv"

    let dbTemplate = pathToDbTemplate ()
    let dbActive = outputFile "structure_relationships.sqlite" |> Path.GetFullPath

    printfn "%s" dbActive
    if File.Exists(dbActive) then
        System.IO.File.Delete dbActive
    else ()
    System.IO.File.Copy(sourceFileName = dbTemplate, destFileName = dbActive)

    let connParams = sqliteConnParamsVersion3 dbActive
    runSqliteDb connParams 
        <| sqliteDb { 
                do! insertAideStructRelationshipRows aideStructRelationshipsCsv
                do! insertAideAssetLookupRows aideAssetLookupsCsv
                return ()
            }
