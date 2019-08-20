// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module Addendum =
    
    open System

    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)


    // ************************************************************************
    // For MarkdownDoc

    let htmlIdAnchor (name : string) (body : Text) : Text = 
        rawtext (sprintf "<a id=\"%s\">" name) ^^ body ^^ rawtext "</a>"

    /// Print a DataTime. 
    /// The output uses FSharp's ToString() so it may be printed in 
    /// exponential notation.
    let dateTimeDoc (datetime : System.DateTime) (format : string) : Text = 
        datetime.ToString(format) |> text

    let iso8601DateTimeDoc (datetime : System.DateTime) : Text = 
        dateTimeDoc datetime "yyyy-MM-dd hh:mm:ss"


    /// Add to markdown-doc?
    let commaSpaceSep (texts : Text list) : Text = 
        match texts with 
        | [] -> emptyText
        | [d1] -> d1
        | d1 :: rest -> List.fold (fun ac d -> ac ^^ character ',' ^+^ d) d1 rest

