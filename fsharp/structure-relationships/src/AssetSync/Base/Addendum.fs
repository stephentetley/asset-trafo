// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.Base


module Addendum =
    
    open SLSqlite.Core
    open MarkdownDoc.Markdown
    open MarkdownDoc.Markdown.InlineHtml

    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)


    // ************************************************************************
    // For sl-sqlite

    // TryHead will be a useful strategy 
    // If the resultset is expected to have 1+ results Head makes sense
    // but if it is expected to have 0 or 1 then TryHead is ueful.

    let optional (action : SqliteDb<'a>) : SqliteDb<'a option> = 
        attempt (action |>> Some) (fun _ -> mreturn None)


    // ************************************************************************
    // For markdown-doc


    /// Span is wrong in MarkdownDoc
    let htmlSpan (attrs : HtmlAttrs) (body : Text) : Text = 
        htmlElement "span" attrs body


    let backgroundColor (value : string) : StyleDecl = 
        StyleDecl("background-color", value)

    type ColourName = string

    let bisque : ColourName = "bisque"
    let gold : ColourName = "gold"
    let lightcoral : ColourName = "lightcoral"
    let palegreen : ColourName = "palegreen"




    
