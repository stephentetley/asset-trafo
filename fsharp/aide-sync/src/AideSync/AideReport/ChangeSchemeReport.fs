// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.AideReport

[<AutoOpen>]
module ChangeSchemeReport =
    
    open System.IO

    open SLSqlite.Core

    open AideSync.Base.Common
    open AideSync.AideReport.Internal.BuildReport
    open AideSync.AideReport.Internal.PrintReport
    open AideSync.AideReport

    type ErrMsg = string
    

    let runChangeSchemeReport (config : AideReportConfig) 
                                (schemeCode : string) 
                                (outputHtmlFile : string) : Result<unit, ErrMsg> = 
        let connParams = 
            let dbActive = config.PathToDb |> Path.GetFullPath
            sqliteConnParamsVersion3 dbActive

        let pandocOpts = pandocHtmlDefaults config.PathToCss

        match runSqliteDb connParams (getChangeScheme schemeCode) with
        | Error msg -> printfn "Fail: %s" msg ; Error "Bad"
        | Ok scheme -> writeFullChangeReport scheme pandocOpts outputHtmlFile

