// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.Base


module Addendum =


    open SLSqlite.Core

    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)

    // ************************************************************************
    // For sl-sqlite ?

    let valueByName (reader : ResultItem) (field : string) : obj = 
        let ix = reader.GetOrdinal(field)
        reader.GetValue(ix)

    let getString (reader : ResultItem) (field : string) : string = 
        let ix = reader.GetOrdinal(field)
        reader.GetString(ix)
