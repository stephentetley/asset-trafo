// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module Addendum =
    
    open SLSqlite.Core
    open MarkdownDoc.Markdown


    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)

    // ************************************************************************
    // For sl-sqlite


    let getString (reader : RowReader) (field : string) : string = 
        let ix = reader.GetOrdinal(field)
        reader.GetString(ix)


    // ************************************************************************
    // For MarkdownDoc

    /// Add to markdown-doc?
    let commaSpaceSep (texts : Text list) : Text = 
        textPunctuate (rawtext ", ") texts
