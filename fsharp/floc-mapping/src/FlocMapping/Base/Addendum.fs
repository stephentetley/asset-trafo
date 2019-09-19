// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Base


module Addendum =


    open SLSqlite.Core

    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)

    // ************************************************************************
    // For sl-sqlite ?

    // TryHead will be a useful strategy 
    // If the resultset is expected to have 1+ results Head makes sense
    // but if it is expected to have 0 or 1 then TryHead is ueful.


    // Added after 1.0.0-alpha-20190919...
    let optional (action : SqliteDb<'a>) : SqliteDb<'a option> = 
        attempt (action |>> Some) (fun _ -> mreturn None)

    // Added after 1.0.0-alpha-20190919...
    let (<|>) (action1 : SqliteDb<'a>) (action2 : SqliteDb<'a>) : SqliteDb<'a> = 
        attempt action1 (fun _ -> action2)

    // Added after 1.0.0-alpha-20190919...
    // Run an action, return true if it succeeds, false if it fails.
    let succeeds (action : SqliteDb<'a>) : SqliteDb<bool> = 
        (action |>> fun _ -> true) <|> mreturn false

