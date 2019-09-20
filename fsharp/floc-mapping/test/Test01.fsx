// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
open System
open System.IO

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

#load "..\src\FlocMapping\Base\Addendum.fs"
#load "..\src\FlocMapping\AibBasis.fs"
#load "..\src\FlocMapping\S4Basis.fs"
#load "..\src\FlocMapping\TranslateFloc.fs"
open FlocMapping.AibBasis
open FlocMapping.S4Basis
open FlocMapping.TranslateFloc

let test01 () = 
    let f1 = makeFloc "NEWMA-CAA-NET-TEL-SYS02"
    let f2 = makeFloc "NEWMA-CAA-NET-TEL-SYS01"
    compare f1 f2

let pathToDb () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\floc_mapping_active.sqlite")

let getConnParams () : SqliteConnParams = 
    let dbActive = pathToDb () |> Path.GetFullPath
    sqliteConnParamsVersion3 dbActive

let runDb (action : SqliteDb<'a>) : Result<'a, ErrMsg> = 
    let conn = getConnParams () 
    runSqliteDb conn action


// demo01 101026176u ;;         should be true
// demo01 10102617u ;;          should be false
let demo01 (refId : uint32) = 
    isS4Equipment refId |> runDb

let demo02 (floc : string) = 
    isS4Site floc |> runDb

// demo03 "SAI00002341" ;;          Ok []
// demo03 "SAI00162177" ;;
let demo03 (sai : string) = 
    aibEquipmentBelowDirect sai |> runDb