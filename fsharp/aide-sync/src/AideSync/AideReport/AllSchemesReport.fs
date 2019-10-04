// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.AideReport

[<AutoOpen>]
module AllSchemesReport =
    
    open System.IO
    open System.Data.SQLite

    open MarkdownDoc.Markdown
    open MarkdownDoc.Markdown.InlineHtml
    open MarkdownDoc.Pandoc

    open SLSqlite.Core

    open AideSync.Base.Common
    open AideSync.AideReport.Internal.Datatypes
    open AideSync.AideReport

    // ************************************************************************
    // Find all schemes query

    /// TODO - maybe should have filter conditions?
    let getAllSchemes () : SqliteDb<ChangeSchemeInfo list> = 
        let sql = 
            """
            SELECT 
                scheme.scheme_id            AS [scheme_id],
                scheme.scheme_code          AS [scheme_code],
                scheme.scheme_name          AS [scheme_name],
                scheme.solution_provider    AS [solution_provider],
                scheme.batch_reference      AS [batch_reference],
                scheme.brm_solution_id      AS [brm_solution_id],
                scheme.brm_completion_year  AS [brm_completion_year]
            FROM work_scheme AS scheme
            ;
            """
        let cmd = 
            new System.Data.SQLite.SQLiteCommand(commandText = sql)
                   
        let readRow (reader : ResultItem) : ChangeSchemeInfo = 
            { SchemeId = reader.GetInt64(0)
            ; SchemeCode = reader.GetString(1)
            ; Name = reader.GetString(2)
            ; SolutionProvider = reader.GetString(3)
            ; BatchReference = reader.TryGetString(4)
            ; BrmSolutionId = reader.TryGetString(5)
            ; BrmCompletionYear = reader.TryGetInt32(6)
            }

        queryCommand cmd (Strategy.ReadAll readRow) <?> "findAllSchemes failed"

    // ************************************************************************
    // Change requests summary

    /// Change Scheme Table 
    let changeSchemeInfosTable (schemes : ChangeSchemeInfo list) : Markdown option = 
        let headings =
            [ alignLeft 30 (headingTitle "Solution Provider Name")
            ; alignLeft 30 (headingTitle "Code")
            ; alignLeft 40 (headingTitle "Name")
            ; alignLeft 40 (headingTitle "Batch Reference")
            ; alignLeft 25 (headingTitle "BRM Solution Id")
            ; alignLeft 25 (headingTitle "BRM Completion Year")
            ]
        
        let cellText (contents : string) : Markdown = 
            contents |> text |> markdownText
        
        let makeRow (scheme : ChangeSchemeInfo) : TableRow = 
            [ scheme.SolutionProvider |> cellText 
            ; scheme.SchemeCode |> cellText
            ; scheme.Name |> cellText
            ; scheme.BatchReference |> Option.defaultValue "" |> cellText
            ; scheme.BrmSolutionId |> Option.defaultValue "" |> cellText
            ; scheme.BrmCompletionYear |> Option.map (fun x -> x.ToString()) |> Option.defaultValue "" |> cellText
            ]
        
        let rows = schemes |> List.map makeRow 
    
        match schemes with
        | [] -> None
        | _ -> makeTableWithHeadings headings rows |> gridTable |> Some


    let changeSchemeTable (changeSchemes : ChangeSchemeInfo list) : Markdown =
        match changeSchemeInfosTable changeSchemes with
        | None -> asterisks (text "No change schemes")  |> h3
        | Some table -> table


    let makeAllSchemesReport (changeSchemes : ChangeSchemeInfo list) : Markdown = 
        h1 (htmlAnchorId "top" (text "AIDE Change Schemes"))
            ^!!^ changeSchemeTable changeSchemes
            ^!!^ linkToTop

    let writeAllSchemesReport (schemes : ChangeSchemeInfo list)
                              (pandocOpts : PandocOptions)
                              (outputHtmlFile : string) : Result<unit, string> = 
        let doc = makeAllSchemesReport schemes
        writeMarkdownReport doc "Aide Change Scheme Report" pandocOpts outputHtmlFile


    let runAllSchemesReport (config : AideReportConfig) 
                            (outputHtmlFile : string) : Result<unit, ErrMsg> = 
        let connParams = 
            let dbActive = config.PathToDb |> Path.GetFullPath
            sqliteConnParamsVersion3 dbActive

        let pandocOpts = pandocHtmlDefaults config.PathToCss

        match runSqliteDb connParams (getAllSchemes ()) with
        | Error msg -> printfn "Fail: %s" msg ; Error "Bad"
        | Ok schemes -> writeAllSchemesReport schemes pandocOpts outputHtmlFile