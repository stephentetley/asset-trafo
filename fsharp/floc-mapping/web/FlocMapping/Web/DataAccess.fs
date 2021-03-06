﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Web


module DataAccess = 
    
    open System.IO

    open SLSqlite.Core
    
    open FlocMapping.AibBasis
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

    let s4CommonNamePath (floc : Floc) : SqliteDb<string> = 
        let sql = 
            """
            WITH RECURSIVE
            temp_table(floc, parent, short_name) AS (
                SELECT '', :floc, ''
                    UNION ALL
                SELECT s4_floc.s4_floc, s4_floc.parent_floc, s4_floc.short_name
                FROM s4_floc, temp_table
                WHERE s4_floc.s4_floc = temp_table.parent
                )
            SELECT 
                floc.short_name         AS [Name]
            FROM temp_table
            JOIN s4_floc    AS floc ON temp_table.floc = floc.s4_floc
            ORDER BY floc.s4_floc
            ;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "floc" (floc.ToString() |> stringParam) 
        
        let readRow (result : ResultItem) : string = result.GetString(0)
        
        queryKeyed cmd (Strategy.Map readRow) |>> String.concat "/"


    let saiFlocMapping (sai : string) : SqliteDb<FlocAnswer list> = 
        let fillout (floc : Floc) : SqliteDb<FlocAnswer> = 
            sqliteDb { 
                let! name = s4FlocName floc
                let! path = s4CommonNamePath floc
                return { S4Floc = floc
                         Description = name 
                         DescriptionPath = path } 
            }
        aibReferenceToS4Floc sai >>=  mapM fillout

    let pliEquipmentAnswers1 (pli : string) : SqliteDb<EquipmentAnswer list> = 
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
              EquipmentDesc = result.GetString(3)
              ParentDescPath = "" }
        
        queryKeyed cmd (Strategy.Map readRow)

    let pliEquipmentAnswers (pli : string) : SqliteDb<EquipmentAnswer list> =
        let fillout (ans : EquipmentAnswer) = 
            sqliteDb { 
                let! path = s4CommonNamePath ans.ParentFloc
                return { ans with ParentDescPath = path }
            }
        pliEquipmentAnswers1 pli >>= mapM fillout

    let flocMapping (code : string) : SqliteDb<FlocMapping> =
        sqliteDb {
            let! commonName = getAibCommonName code |>> Option.defaultValue ""
            let! flocAnswers = 
                match isPliCode code with
                | true -> pliEquipmentAnswers code |>> List.map EquipmentAns
                | false -> saiFlocMapping code |>> List.map FlocAns
            return { AibReference = code
                     AibCommonName = commonName
                     MappingAnswers = flocAnswers }
        }