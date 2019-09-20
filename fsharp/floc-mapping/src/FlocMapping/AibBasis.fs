// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping

module AibBasis =

    open SLSqlite.Core

    open FlocMapping.Base.Addendum

    
    type AibCategory = 
        | AibInstallation
        | AibProcessGroup
        | AibProcess
        | AibPlant
        | AibPlantItem
        | AibEquipment

    let internal decodeAibCategory (categoryName : string) : AibCategory option = 
        match categoryName with
        | "INSTALLATION" -> Some AibInstallation
        | "PROCESS GROUP" -> Some AibProcessGroup
        | "PROCESSS" -> Some AibProcess
        | "PLANT" -> Some AibPlant
        | "PLANT ITEM" -> Some AibPlantItem
        | "EQUIPMENT" -> Some AibEquipment
        | _ -> None


    let private testSai (category : string) (sai : string) : SqliteDb<bool>= 
        let sql = 
            """
            SELECT 'true' 
            FROM aib_floc AS floc 
            WHERE floc.sai_ref = :sai
            AND   floc.category = :category;
            """

        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "sai" (stringParam sai)
                |> addNamedParam "category" (stringParam category)
        
        let readRow1 (result : ResultItem) : string = result.GetString(0)
        
        queryKeyed cmd (Strategy.Head readRow1) |> succeeds
    
    let isAibInstallation (saicode : string) : SqliteDb<bool> = 
        testSai "INSTALLATION" saicode

    let isAibProcessGroup (saicode : string) : SqliteDb<bool> = 
        testSai "PROCESS GROUP" saicode

    let isAibProcess (saicode : string) : SqliteDb<bool> = 
        testSai "PROCESS" saicode
    
    let isAibPlant (saicode : string) : SqliteDb<bool> = 
        testSai "PLANT" saicode
    
    let isAibPlantItem (saicode : string) : SqliteDb<bool> = 
        testSai "PLANT ITEM" saicode


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

    let getAibCategory (saicode : string) : SqliteDb<AibCategory option> = 
        let sql = 
            """
            SELECT floc.category
            FROM aib_floc AS floc
            WHERE floc.sai_ref = :saicode;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "saicode" (stringParam saicode)
        
        let readRow (result : ResultItem) : AibCategory option = 
            result.GetString(0) |> decodeAibCategory
        
        queryKeyed cmd (Strategy.Head readRow) <|> mreturn None