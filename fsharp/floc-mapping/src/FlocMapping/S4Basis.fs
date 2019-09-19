// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping

module S4Basis =


    open SLSqlite.Core

    open FlocMapping.Base.Addendum

    [<Struct>]
    type Floc = 
        internal | Floc of string []

        override x.ToString () = 
            let (Floc arr) = x in String.concat "-" arr

        member x.Depth
            with get () = let (Floc arr) = x in arr.Length

    let makeFloc (floc : string) = 
        Floc(floc.Split([| '-' |]))

    let flocIsPrefix (floc : Floc) (subfloc : Floc) : bool = 
        let (Floc arrFloc) = floc
        let (Floc arrSub) = subfloc
        let rec work (ix : int) cont = 
            if ix >= arrSub.Length then 
                // Success - arrSub has been traversed
                cont true
            else if ix >= arrFloc.Length then
                cont false
            else if arrSub.[ix] <> arrFloc.[ix] then
                cont false
            else
                work (ix + 1) cont
        work 0 (fun x -> x)
                
    

    let private queryFloc (table : string) (floc : string) : SqliteDb<bool>= 
        let sql = 
            sprintf "SELECT 'true' FROM %s AS t1 WHERE t1.s4_floc = :floc;" table

        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "floc" (stringParam floc)
        
        let readRow1 (result : ResultItem) : string = result.GetString(0)
        
        queryKeyed cmd (Strategy.Head readRow1) |> succeeds
    
    let isS4Site (floc : string) : SqliteDb<bool> = 
        queryFloc "s4_site" floc

    let isS4Function (floc : string) : SqliteDb<bool> = 
        queryFloc "s4_function" floc

    let isS4ProcessGroup (floc : string) : SqliteDb<bool> = 
        queryFloc "s4_process_group" floc

    let isS4Process (floc : string) : SqliteDb<bool> = 
        queryFloc "s4_process" floc
    
    let isS4System (floc : string) : SqliteDb<bool> = 
        queryFloc "s4_system" floc
    
    let isS4Assembly (floc : string) : SqliteDb<bool> = 
        queryFloc "s4_assembly" floc

    let isS4Item (floc : string) : SqliteDb<bool> = 
        queryFloc "s4_item" floc
    
    let isS4Component (floc : string) : SqliteDb<bool> = 
        queryFloc "s4_component" floc

    let isS4Equipment (reference : uint32) : SqliteDb<bool> = 
        let sql = 
            """
            SELECT 'true'
            FROM s4_equipment AS equip
            WHERE equip.s4_ref = :equip_ip;
            """
        let cmd = 
            new KeyedCommand (commandText = sql)
                |> addNamedParam "equip_ip" (uint32Param reference)
        
        let readRow1 (result : ResultItem) : string = result.GetString(0)
        
        queryKeyed cmd (Strategy.Head readRow1) |> succeeds
