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

#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20190822\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core


#load "..\src\AssetSync\StructureRelationships\PopulateDb.fs"
open AssetSync.StructureRelationships.PopulateDb


let dbFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output\", relativePath)


// e.g test01 2111881L ;;
let test01 (startId : int64) = 
    let dbActive = dbFile "structure_relationships.sqlite" |> Path.GetFullPath
    let connParams = sqliteConnParamsVersion3 dbActive
    let sql = 
        """
        WITH RECURSIVE
        temp_table(child_id) AS (
            SELECT :start_id
                UNION ALL
            SELECT aide_structure_relationships.child_id
            FROM aide_structure_relationships, temp_table
            WHERE aide_structure_relationships.parent_id = temp_table.child_id
            )
            SELECT
                temp_table.child_id AS [ChildId],
                aide_asset_lookups.asset_common_name AS [CommonName]
            FROM temp_table
            JOIN aide_asset_lookups    ON temp_table.child_id = aide_asset_lookups.aide_asset_id
            ORDER BY aide_asset_lookups.asset_common_name
            ;
        """

    let cmd = new SQLiteCommand(commandText = sql)
    cmd.Parameters.AddWithValue(parameterName = "start_id", value = box startId) |> ignore
        
    let readRow1 (reader : RowReader) : int64 * string = 
        let key = reader.GetInt64(0)
        let path = reader.GetString(1) 
        (key, path)

    runSqliteDb connParams 
        <| sqliteDb { 
                return! executeReader cmd (readerReadAll readRow1)
            }
