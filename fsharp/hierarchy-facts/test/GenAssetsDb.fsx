﻿// Copyright (c) Stephen Tetley 2019

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

// Need SLFormat and FactX even though they aren't directly referenced
#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20190823\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core

#I @"C:\Users\stephen\.nuget\packages\factx\1.0.0-alpha-20190721\lib\netstandard2.0"
#r "FactX"


#load "..\src\AssetSync\Base\Addendum.fs"
#load "..\src\AssetSync\Base\FactsCommon.fs"
#load "..\src\AssetSync\Base\DbExportSchema.fs"
#load "..\src\AssetSync\SQLiteFacts\PopulateAssetsDb.fs"
open AssetSync.SQLiteFacts.PopulateAssetsDb



let outputDbFile () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\assets.sqlite")

let pathToDbTemplate () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\ddl\assets_template.sqlite")



let main () : Result<unit, ErrMsg> = 
    let s4FlocCsv = @"G:\work\Projects\asset_sync\S4_Floc_Mapping_Site-A-Z_General_Structure_Initial.csv"
    let s4EquipmentCsv = @"G:\work\Projects\asset_sync\equipment_migration_s1.csv"
    let aibFlocCsv = @"G:\work\Projects\asset_sync\rules\aib_floc_extract4.csv"
    let aibEquipCsv = @"G:\work\Projects\asset_sync\rules\aib_equipment_extract2.csv"

    // Copy template
    let dbTemplate = pathToDbTemplate ()
    let dbActive = outputDbFile () |> Path.GetFullPath
    printfn "Output: %s" dbActive
    if File.Exists(dbActive) then
        System.IO.File.Delete dbActive
    else ()
    System.IO.File.Copy(sourceFileName = dbTemplate, destFileName = dbActive)

    let connParams = sqliteConnParamsVersion3 dbActive
    runSqliteDb connParams 
        <| sqliteDb { 
                printfn "S4 Hierarchy..."
                do! insertS4FlocRecords s4FlocCsv
                printfn "S4 Equipment..."
                do! insertS4EquipmentRecords s4EquipmentCsv
                printfn "Aib Hierarchy..."
                do! insertAibFlocRecords aibFlocCsv
                printfn "Aib Equipment..."
                do! insertAibEquipmentRecords aibEquipCsv
                printfn "Links..."
                do! insertLinks s4FlocCsv
                return ()
            }


    
