// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.Base


module Addendum =
    
    open SLSqlite.Core
    open MarkdownDoc.Markdown


    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)

    // ************************************************************************
    // For sl-sqlite

    let valueByName (reader : RowReader) (field : string) : obj = 
        let ix = reader.GetOrdinal(field)
        reader.GetValue(ix)

    let getString (reader : RowReader) (field : string) : string = 
        let ix = reader.GetOrdinal(field)
        reader.GetString(ix)


    // ************************************************************************
    // For MarkdownDoc

    /// Add to markdown-doc?
    let commaSpaceSep (texts : Text list) : Text = 
        textPunctuate (rawtext ", ") texts
