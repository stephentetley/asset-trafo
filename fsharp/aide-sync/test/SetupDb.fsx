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



#load "..\src\AideSync\ImportSchema.fs"
#load "..\src\AideSync\PopulateDb.fs"
open AideSync.PopulateDb

let sourceCsv (fileName : string) : string = 
    Path.Combine(@"G:\work\Projects\asset_sync\aide_report", fileName)

let pathToDbTemplate () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\ddl\aide_sync.sqlite")

let outputDbFile () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data\db\aide_sync_active.sqlite")


type ErrMsg = string

let main () : Result<unit, ErrMsg> = 
    let workSchemeCsv       = sourceCsv "change_requests_schemes_20190902.csv"
    let changeRequestsCsv   = sourceCsv "change_requests_change_reqs_20190902.csv"
    let aiAssetsCsv         = sourceCsv "ai_asset_20190902.csv"
    let aideAssetsCsv       = sourceCsv "aide_asset_20190902.csv"
    let assetChangesCsv     = sourceCsv "change_requests_assets_20190902.csv"
    let attrChangesCsv      = sourceCsv "change_requests_attributes_20190903.csv"
    let repAttrChangesCsv   = sourceCsv "change_requests_repeated_attributes_20190902.csv"
    let newAssetsCsv        = sourceCsv "no_change_req_new_aide_assets_20190902.csv"
    let newAttributesCsv    = sourceCsv "no_change_req_new_aide_attribute_values_20190902.csv"
    let aideStructRelsCsv   = sourceCsv "structure_relationships_aide_20190902.csv"
    let aiStructRelsCsv     = sourceCsv "structure_relationships_ai_20190902.csv"

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
                do! insertWorkSchemeRows workSchemeCsv
                do! insertChangeRequestRows changeRequestsCsv
                do! insertAiAssetRows aiAssetsCsv
                do! insertAideAssetRows aideAssetsCsv
                do! insertAssetChangeRows assetChangesCsv
                do! insertAssetNewRows newAssetsCsv
                do! insertAttributeChangeRows attrChangesCsv
                do! insertAttributeNewRows newAttributesCsv
                do! insertRepeatedAttributeChangeRows repAttrChangesCsv
                do! insertAideStructRelationshipRows aideStructRelsCsv
                do! insertAiStructRelationshipRows aiStructRelsCsv
                return ()
            }

