// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
open System
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.1.1\lib\netstandard2.0"
#r "FSharp.Data.dll"
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

#I @"C:\Users\stephen\.nuget\packages\slsqlite\1.0.0-alpha-20190820\lib\netstandard2.0"
#r "SLSqlite.dll"
open SLSqlite.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"
open SLFormat.CommandOptions.CommandOptions

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20190821a\lib\netstandard2.0"
#r "MarkdownDoc.dll"
open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc

#load "..\src\AssetSync\ChangesReport\Addendum.fs"
#load "..\src\AssetSync\ChangesReport\Datatypes.fs"
open AssetSync.ChangesReport.Addendum
open AssetSync.ChangesReport.Datatypes


let outputFile (relFileName : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output", relFileName)

let getAssetPropertyChange (answerRow : RowReader) 
                            (propertyDescription : string)
                            (leftField : string) 
                            (rightField : string) : AssetProperty option = 
    let name1 = getString answerRow leftField
    let name2 = getString answerRow rightField
    if name1 <> name2 then
        { PropertyName = propertyDescription
        ; AiValue = name1
        ; AideValue = name2
        } |> Some
    else None

let test01 () = 
    let dbActive = outputFile "change_requests.sqlite" |> Path.GetFullPath
    let connParams = sqliteConnParamsVersion3 dbActive
    
    let sql = 
        "SELECT \
                cr_asset.change_request_id     AS chreq_id, \
                cr_asset.* \
        FROM        change_request_asset AS cr_asset \
        WHERE \
                cr_asset.change_request_id = 148395 \
        ;"

    let cmd = new SQLiteCommand(commandText = sql)
    //cmd.Parameters.AddWithValue(parameterName = "start_id", value = box startId) |> ignore
        
    let readRow1 (reader : RowReader)   = 
        let key = reader.GetInt64(0)
        let change1 = getAssetPropertyChange reader "Common Name" "ai_common_name" "aide_common_name"
        let change2 = getAssetPropertyChange reader "Manufacturer" "ai_manufacturer" "aide_manufacturer"
        (key, [change1; change2])

    runSqliteDb connParams 
        <| sqliteDb { 
                return! executeReader cmd (readerReadAll readRow1)
            }
