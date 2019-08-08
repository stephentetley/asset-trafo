// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
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

#load "..\src\AssetTrafo\Base\Common.fs"
#load "..\src\AssetTrafo\Base\SqliteConn.fs"
#load "..\src\AssetTrafo\Base\DbExportSchema.fs"
#load "..\src\AssetTrafo\SQLiteFacts\PopulateAssetsDb.fs"
open AssetTrafo.Base.Common
open AssetTrafo.Base.SqliteConn
open AssetTrafo.SQLiteFacts.PopulateAssetsDb








let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\output\", relativePath)

let pathToDbTemplate () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\data\", "assets_db_template.sqlite")




let main () : Result<unit, ErrMsg> = 
    let s4FlocCsv = @"G:\work\Projects\asset_sync\S4_Floc_Mapping_Site-A-Z_General_Structure_Initial.csv"
    let s4EquipmentCsv = @"G:\work\Projects\asset_sync\equipment_migration_s1.csv"
    let aibFlocCsv = @"G:\work\Projects\asset_sync\rules\aib_floc_extract4.csv"
    let aibEquipCsv = @"G:\work\Projects\asset_sync\rules\aib_equipment_extract2.csv"

    // Copy template
    let dbTemplate = pathToDbTemplate ()
    let dbActive = outputFile "assets.sqlite" |> Path.GetFullPath
    printfn "%s" dbActive
    if File.Exists(dbActive) then
        System.IO.File.Delete dbActive
    else ()
    System.IO.File.Copy(sourceFileName = dbTemplate, destFileName = dbActive)

    let connParams = sqliteConnParamsVersion3 dbActive
    runSqliteConnection connParams 
        <| sqliteConn { 
                do! insertS4FlocRecords s4FlocCsv
                do! insertS4EquipmentRecords s4EquipmentCsv
                do! insertAibFlocRecords aibFlocCsv
                do! insertAibEquipmentRecords aibEquipCsv
                return ()
            }

let temp01 ()  = 
    let longestFloc (acc : int) (row: S4FlocRow) : int = 
        match row.``L8_Floc Code`` with
        | None -> acc
        | Some str -> max acc (str.Length)
        
    let s4FlocCsv = @"G:\work\Projects\asset_sync\S4_Floc_Mapping_Site-A-Z_General_Structure_Initial.csv"
    getS4FlocRows s4FlocCsv 
        |> Seq.fold longestFloc 0