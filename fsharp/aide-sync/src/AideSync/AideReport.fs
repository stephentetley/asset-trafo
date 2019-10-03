// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module AideReport =
    
    open System.IO

    open SLSqlite.Core

    open AideSync.AideReport.BuildReport
    open AideSync.AideReport.PrintReport

    type ErrMsg = string

    type AideReportConfig = 
        { PathToCss : string 
          PathToDb : string }


    let runChangeSchemeReport (config : AideReportConfig) 
                                (schemeCode : string) 
                                (outputHtmlFile : string) : Result<unit, ErrMsg> = 
        let connParams = 
            let dbActive = config.PathToDb |> Path.GetFullPath
            sqliteConnParamsVersion3 dbActive

        let pandocOpts = pandocHtmlDefaults config.PathToCss

        match runSqliteDb connParams (getChangeScheme schemeCode) with
        | Error msg -> printfn "Fail: %s" msg ; Error "Bad"
        | Ok scheme -> writeFullReport scheme pandocOpts outputHtmlFile

