// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping

module TranslateFloc =

    open SLSqlite.Core

    open FlocMapping.S4Basis

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
        
        let readRow (result : ResultItem) : string = result.GetString(0)
        
        queryKeyed cmd (Strategy.Map readRow) 


    let aibFunLocsBelow (saicode : string) : SqliteDb<string list> = 
        let sql = 
            """
            WITH RECURSIVE
            temp_table(sai_ref) AS (
                SELECT :saicode
                    UNION ALL
                SELECT aib_floc.sai_ref
                FROM aib_floc, temp_table
                WHERE aib_floc.parent_ref = temp_table.sai_ref
                )
            SELECT 
                floc.sai_ref            AS [Reference]
            FROM temp_table
            JOIN aib_floc    AS floc ON temp_table.sai_ref = floc.sai_ref
            ORDER BY floc.common_name
            ;
            
            SELECT floc.* FROM aib_floc AS floc WHERE floc.sai_ref = :saicode;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "saicode" (stringParam saicode) 
        
        let readRow (result : ResultItem) : string = result.GetString(0)
        
        queryKeyed cmd (Strategy.Map readRow) 


    let aibEquipmentBelow (saicode : string) : SqliteDb<string list> = 
        sqliteDb { 
            let! funlocs = aibFunLocsBelow saicode |>> (fun xs -> saicode :: xs)
            return! mapM aibEquipmentBelowDirect funlocs |>> List.concat
        }

    
    let aibToS4Direct (saicode : string) : SqliteDb<Floc list> = 
        let sql = 
            """
            SELECT link.s4_floc
            FROM s4_aib_reference AS link
            WHERE link.aib_ref = :saicode;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "saicode" (stringParam saicode) 
        
        let readRow (result : ResultItem) : Floc = 
            result.GetString(0) |> makeFloc
        
        queryKeyed cmd (Strategy.Map readRow)
    
    let minimizeFlocAnswers (flocs : Floc list) : Floc list = 
        let rec work xs cont = 
            match xs with
            | [] -> cont []
            | item :: rest ->
                let smaller = List.filter (fun x -> not (flocIsPrefix item x)) rest
                work smaller (fun xs -> 
                cont (item :: xs))
        let orderedFlocs = flocs |> List.sortBy (fun x -> x.ToString())
        work orderedFlocs (fun x -> x)

    let aibReferenceToS4Floc (saicode : string) : SqliteDb<Floc list> = 
        sqliteDb { 
            match! aibToS4Direct saicode with
            | [] ->
                let! equipments = aibEquipmentBelow saicode
                let! flocs = mapM aibToS4Direct equipments |>> List.concat
                return (minimizeFlocAnswers flocs)
            | xs -> return xs
            
        }
        