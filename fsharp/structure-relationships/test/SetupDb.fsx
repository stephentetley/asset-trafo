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

#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20190823\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core


#load "..\src\AssetSync\StructureRelationships\PopulateDb.fs"
open AssetSync.StructureRelationships.PopulateDb


let outputDbFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\", relativePath)

let pathToDbTemplate () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\ddl\structure_relationships.sqlite")


type ErrMsg = string

let main () : Result<unit, ErrMsg> = 
    let aideStructRelationshipsCsv = 
        @"G:\work\Projects\asset_sync\aide_report\structure_relationships_aide_20190822.csv"

    let aideAssetLookupsCsv = 
        @"G:\work\Projects\asset_sync\aide_report\structure_relationships_aide_lookups_20190822.csv"

    let aiStructRelationshipsCsv = 
        @"G:\work\Projects\asset_sync\aide_report\structure_relationships_ai_20190822.csv"

    let aiAssetLookupsCsv = 
        @"G:\work\Projects\asset_sync\aide_report\structure_relationships_ai_lookups_20190822.csv"


    let dbTemplate = pathToDbTemplate ()
    let dbActive = outputDbFile "structure_relationships.sqlite" |> Path.GetFullPath

    printfn "%s" dbActive
    if File.Exists(dbActive) then
        System.IO.File.Delete dbActive
    else ()
    System.IO.File.Copy(sourceFileName = dbTemplate, destFileName = dbActive)

    let connParams = sqliteConnParamsVersion3 dbActive

    

    runSqliteDb connParams 
        <| sqliteDb { 
                let cmd1 = new SQLiteCommand (commandText = "PRAGMA synchronous = OFF")
                let! _ = executeNonQuery cmd1
                let cmd2 = new SQLiteCommand (commandText = "PRAGMA journal_mode = MEMORY")
                let! _ = executeNonQuery cmd2
                do! insertAiStructRelationshipRows aiStructRelationshipsCsv
                do! insertAiAssetLookupRows aiAssetLookupsCsv
                do! insertAideStructRelationshipRows aideStructRelationshipsCsv
                do! insertAideAssetLookupRows aideAssetLookupsCsv
                return ()
            }
