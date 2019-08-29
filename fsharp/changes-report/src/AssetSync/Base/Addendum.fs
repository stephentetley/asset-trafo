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
    // For sl-sqlite ?

    let valueByName (reader : ResultItem) (field : string) : obj = 
        let ix = reader.GetOrdinal(field)
        reader.GetValue(ix)

    let getString (reader : ResultItem) (field : string) : string = 
        let ix = reader.GetOrdinal(field)
        reader.GetString(ix)


    // ************************************************************************
    // For MarkdownDoc

    /// Add to markdown-doc?
    let commaSpaceSep (texts : Text list) : Text = 
        textPunctuate (rawtext ", ") texts


    let htmlElement (name : string) (attrs : HtmlAttrs) (body : Text) = 
        let startTag = 
            match attrs |> List.map (fun x -> x.Text) with
            | [] -> rawtext name |> angleBrackets
            | xs -> rawtext name ^+^ hsep xs |> angleBrackets
        let endTag = rawtext <| sprintf "</%s>" name
        startTag ^^ body ^^ endTag


    /// Span is wrong in MarkdownDoc
    let htmlSpan (attrs : HtmlAttrs) (body : Text) : Text = 
        htmlElement "span" attrs body

        /// a id is wrong in MarkdownDoc
    /// ``<a id="anchorName">This is an anchor</a>``
    let htmlIdAnchor (name : string) (body : Text) : Text = 
        htmlElement "a" [ HtmlAttr("id", name) ] body

    let backgroundColor (value : string) : StyleDecl = 
        StyleDecl("background-color", value)



    type ColorName = string

    let bisque : ColorName = "bisque"
    let gold : ColorName = "gold"
    let lightcoral : ColorName = "lightcoral"
    let palegreen : ColorName = "palegreen"