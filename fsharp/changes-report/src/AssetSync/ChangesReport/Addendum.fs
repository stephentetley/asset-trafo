﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module Addendum =
    
    open MarkdownDoc.Markdown

    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)


    // ************************************************************************
    // For MarkdownDoc

    /// Add to markdown-doc?
    let commaSpaceSep (texts : Text list) : Text = 
        textPunctuate (rawtext ", ") texts
