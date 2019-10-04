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


#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20191003\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core


#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"
open SLFormat.CommandOptions
open SLFormat.CommandOptions.SimpleInvoke

#load "..\src\AideSync\SetupDb\ImportSchema.fs"
#load "..\src\AideSync\SetupDb\PopulateDb.fs"
#load "..\src\AideSync\SetupDb.fs"
open AideSync.SetupDb

let pathtoCsvExports () : string = 
    let current = "20190925"
    Path.Combine(@"G:\work\Projects\asset_sync\aide_sync", current)

let pathToDbTemplate () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\ddl\aide_sync.sqlite")

let outputDbFile () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\aide_sync_active.sqlite")


let main () : Result<unit, ErrMsg> = 
    let config : SetupDbConfig = 
        { ExportsSourceFolder = pathtoCsvExports ()
          DbTemplatePath = pathToDbTemplate ()
          DbOutputPath = outputDbFile ()
        }
    setupDb config

