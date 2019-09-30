// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module BuildReport2 =
    
    open System.Data.SQLite

    open SLSqlite.Core

    open AideSync.Base.Addendum
    open AideSync.Datatypes2
    open AideSync.StructureDiff 
    open AideSync.BasicQueries2      
    

    // ************************************************************************
    // Change Scheme Info

    let getChangeSchemeInfo (schemeCode: string) : SqliteDb<ChangeSchemeInfo> =
        let sql = 
            """
            SELECT 
                        scheme.scheme_id            AS [scheme_id],
                        scheme.scheme_code          AS [scheme_code],
                        scheme.scheme_name          AS [scheme_name],
                        scheme.solution_provider    AS [solution_provider]
            FROM work_scheme    AS scheme
            WHERE scheme.scheme_code = :schemecode
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "schemecode" (stringParam schemeCode) 
                   
        let readRow (reader : ResultItem) : ChangeSchemeInfo = 
            { SchemeId = reader.GetInt64(0)
            ; SchemeCode = reader.GetString(1)
            ; Name = reader.GetString(2)
            ; SolutionProvider = reader.GetString(3)
            }

        queryKeyed cmd (Strategy.Head readRow) 

    // ************************************************************************
    // Change Request ids

    let getSchemeChangeRequestIds (schemeCode: string) : SqliteDb<int64 list> =
        let sql = 
            """
            SELECT 
                    changes.change_request_id       AS [change_request_id]
            FROM    view_scheme_change_requests     AS changes
            JOIN    work_scheme    AS scheme    ON changes.scheme_id = scheme.scheme_id
            WHERE 
                    scheme.scheme_code = :schemecode
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "schemecode" (stringParam schemeCode) 
                   
        let readRow1 (reader : ResultItem) : int64 = 
            reader.GetInt64(0)
            
        queryKeyed cmd (Strategy.ReadAll readRow1)


    // ************************************************************************
    // Change Request Info

    let getChangeRequestInfo (chreqId : int64) : SqliteDb<ChangeRequestInfo> =
        let sql = 
            """
            SELECT 
                    chreq.change_request_id        AS [change_request_id],
                    chreq.change_request_type      AS [request_type],
                    chreq.change_request_status    AS [request_status],
                    chreq.comments                 AS [comment],
                    chreq.change_request_time      AS [request_time]
            FROM    change_request AS chreq
            WHERE
                    chreq.change_request_id = :chreqid
            ;
            """
        let cmd = 
            new KeyedCommand(commandText = sql)
                |> addNamedParam "chreqid" (int64Param chreqId) 
                   
        let readRow (reader : ResultItem) : ChangeRequestInfo = 
            { ChangeRequestId = reader.GetInt64(0)
            ; RequestType = reader.GetString(1)
            ; Status = reader.GetString(2)
            ; Comment = reader.GetString(3)
            ; RequestTime = reader.GetDateTime(4)
            }

        queryKeyed cmd (Strategy.Head readRow)


    // ************************************************************************
    // Find ai asset references for a change request

    /// Assumption - at most one index for a key
    /// The references are the AI uid, not the AIDE uid
    let getChangeRequestAIRootIds (changeRequestId : int64) : SqliteDb<int64 list> = 
        let sql = 
            """
            SELECT 
                    change.ai_asset_id      AS [AssetId]
            FROM    asset_change            AS change
            WHERE
                    change.change_request_id = :chreq
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "chreq" (int64Param changeRequestId)
        
        let readRow (result : ResultItem) : int64 = result.GetInt64(0)

        queryKeyed cmd (Strategy.ReadAll readRow) 


    // ************************************************************************
    // Duild Structure diffs


    let buildStructureDiffs (changeRequestId : int64) 
                            (aiAssetId : int64) : SqliteDb<FlocDiff list>  = 
            sqliteDb {
                let! aiTree = findAiDescendants aiAssetId
                let! aideId = findAideAssetId changeRequestId aiAssetId |> getOptional
                let! aideTree = findAideDescendants aideId
                return diffLists aiTree aideTree
            }
    
       