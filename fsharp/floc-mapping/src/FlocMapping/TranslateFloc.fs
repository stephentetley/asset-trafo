// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping

module TranslateFloc =

    open SLSqlite.Core

    let aibEquipmentBelowDirect (saicode : string) : SqliteDb<string list> = 
        let sql = 
            """
            SELECT t1.pli_ref 
            FROM aib_equipment AS t1 
            WHERE t1.parent_ref = :saicode;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "saicode" (stringParam saicode)
        
        let readRow1 (result : ResultItem) : string = result.GetString(0)
        
        queryKeyed cmd (Strategy.Map readRow1) 

    let private queryChild (table : string) (sai : string) : SqliteDb<string list>= 
        let sql = 
            sprintf "SELECT t1.sai_ref FROM %s AS t1 WHERE t1.parent_ref = :parent;" table

        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "parent" (stringParam sai)
        
        let readRow1 (result : ResultItem) : string = result.GetString(0)
        
        queryKeyed cmd (Strategy.Map readRow1)

    //let aibFunLocsBelow (sai : string) : SqliteDb<string list> = 
    //    queryChild 