// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Web


module DataAccess = 
    
    open System.IO

    open SLSqlite.Core
    
    open FlocMapping.S4Basis
    open FlocMapping.TranslateFloc
    open FlocMapping.Web.Base
    open FlocMapping.Web.Model

    let private pathToDb () : string = 
        Path.Combine(__SOURCE_DIRECTORY__, @"..\..\..\data\db\floc_mapping_active.sqlite")
    
    let private  getConnParams () : SqliteConnParams = 
        let dbActive = pathToDb () |> Path.GetFullPath
        sqliteConnParamsVersion3 dbActive
    
    
    let runDb (action : SqliteDb<'a>) : Result<'a, ErrMsg> = 
        let conn = getConnParams () 
        runSqliteDb conn action


    let s4FlocName (floc : Floc) : SqliteDb<string> = 
        getS4FlocName floc |>> Option.defaultValue ""


    let saiFlocMapping (sai : string) : SqliteDb<FlocAnswer list> = 
        aibReferenceToS4Floc sai >>= 
            mapM (fun x -> s4FlocName x |>> fun name -> { S4Floc = x; Description = name } )

    let pliEquipmentAnswers (pli : string) : SqliteDb<EquipmentAnswer list> = 
        let sql = 
            """
            SELECT 
                floc.s4_floc AS [ParentFloc],
                floc.short_name AS [ParentName],
                links.s4_equip AS [EquipmentUid], 
                equip.short_name AS [EquipmentName]
            FROM pli_ref_to_s4_equip AS links
            JOIN s4_equipment AS equip ON links.s4_equip = equip.s4_ref
            JOIN s4_floc AS floc ON equip.parent_floc = floc.s4_floc
            WHERE links.pli_ref = :pli;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "pli" (stringParam pli) 
        
        let readRow (result : ResultItem) : EquipmentAnswer = 
            { ParentFloc = result.GetString(0) |> makeFloc
              ParentDesc = result.GetString(1)
              EquipmentId = result.GetInt64(2)
              EquipmentDesc = result.GetString(3) }
        
        queryKeyed cmd (Strategy.Map readRow)

    let flocMapping (code : string) : SqliteDb<LookupAnswer list> = 
        match isPliCode code with
        | true -> pliEquipmentAnswers code |>> List.map EquipmentAns
        | false -> saiFlocMapping code |>> List.map FlocAns