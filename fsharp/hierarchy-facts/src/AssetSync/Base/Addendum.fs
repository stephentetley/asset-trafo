// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.Base


module Addendum =
    
    open System

    open System.Data.SQLite

    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)


    // ************************************************************************
    // SLSqlite

    
    let nullParam () : SQLiteParameter = 
        let param1 = new SQLiteParameter()
        param1.IsNullable <- true
        param1.Value <- null
        param1