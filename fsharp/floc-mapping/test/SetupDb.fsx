// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"        // required for FSharp.Data
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



#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20190919\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core

#load "..\src\FlocMapping\SetupDb\ImportSchema.fs"
#load "..\src\FlocMapping\SetupDb\Populate.fs"
open FlocMapping.SetupDb.ImportSchema
open FlocMapping.SetupDb.Populate


let sourceCsv (fileName : string) : string = 
    Path.Combine(@"G:\work\Projects\asset_sync\floc_mapping", fileName)

let pathToDbTemplate () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\ddl\floc_mapping.sqlite")

let outputDbFile () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\floc_mapping_active.sqlite")


let main () = 
    

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
                do! insertAibFlocRows (sourceCsv "aib_floc_extract4.csv")
                do! insertAibEquipmentRows (sourceCsv "aib_equipment_extract2.csv")
                return ()
            }