// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.Base


module Addendum =


    open SLSqlite.Core

    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)

    // ************************************************************************
    // For sl-sqlite ?

    // TryHead will be a useful strategy 
    // If the resultset is expected to have 1+ results Head makes sense
    // but if it is expected to have 0 or 1 then TryHead is ueful.

    let optional (action : SqliteDb<'a>) : SqliteDb<'a option> = 
        attempt (action |>> Some) (fun _ -> mreturn None)

    let valueByName (reader : ResultItem) (field : string) : obj = 
        let ix = reader.GetOrdinal(field)
        reader.GetValue(ix)

    let getString (reader : ResultItem) (field : string) : string = 
        let ix = reader.GetOrdinal(field)
        reader.GetString(ix)
