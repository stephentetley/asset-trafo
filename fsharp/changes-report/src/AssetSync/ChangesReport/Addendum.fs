﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module Addendum =
    
    open System

    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)


    // ************************************************************************
    // SLSquite

    
    // SQLite has no native date type, represent data times as strings
    // in ISO 8601 format


    let toIso8601String (dt : DateTime) : string = 
        dt.ToString(format = "yyyy-MM-ddThh:mm:ss")

    let parseIso8601String (source : string) : DateTime = 
        DateTime.ParseExact(s = source, format = "yyyy-MM-ddThh:mm:ss", provider = Globalization.CultureInfo.InvariantCulture)

    let tryParseIso8601String (source : string) : DateTime option = 
        try
            parseIso8601String source |> Some
        with
        | _ -> None


