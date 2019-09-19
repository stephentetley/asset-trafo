// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping

module AibBasis =

    open SLSqlite.Core

    open FlocMapping.Base.Addendum

    
                
    

    let private queryFloc (table : string) (sai : string) : SqliteDb<bool>= 
        let sql = 
            sprintf "SELECT 'true' FROM %s AS t1 WHERE t1.sai_ref = :floc;" table

        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "floc" (stringParam sai)
        
        let readRow1 (result : ResultItem) : string = result.GetString(0)
        
        queryKeyed cmd (Strategy.Head readRow1) |> succeeds
    
    let isAibInstallation (saicode : string) : SqliteDb<bool> = 
        queryFloc "aib_installation" saicode

    let isAibProcessGroup (saicode : string) : SqliteDb<bool> = 
        queryFloc "aib_process_group" saicode

    let isAibProcess (saicode : string) : SqliteDb<bool> = 
        queryFloc "aib_process" saicode
    
    let isAibPlant (saicode : string) : SqliteDb<bool> = 
        queryFloc "aib_plant" saicode
    
    let isAibPlantItem (saicode : string) : SqliteDb<bool> = 
        queryFloc "aib_plant_item" saicode


    let isAibEquipment (plicode : string) : SqliteDb<bool> = 
        let sql = 
            """
            SELECT 'true'
            FROM aib_equipment AS equip
            WHERE equip.pli_ref = :plicode;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "plicode" (stringParam plicode)
        
        let readRow1 (result : ResultItem) : string = result.GetString(0)
        
        queryKeyed cmd (Strategy.Head readRow1) |> succeeds
