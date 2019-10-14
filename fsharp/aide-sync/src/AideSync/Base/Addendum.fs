// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.Base


module Addendum =

    open System.Text
    open System.Security.Cryptography
    open SLSqlite.Core

    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)


    // For markdown-doc

    /// Use this to generate ids for links 
    let md5Hash (source : string) : string = 
        use (md5 : MD5) = MD5.Create()
        let codeArray : byte [] = md5.ComputeHash(Encoding.UTF8.GetBytes(source))
        
        Array.fold (fun (sb:StringBuilder) (a:byte) -> sb.AppendFormat(a.ToString("X2"))) (new StringBuilder ()) codeArray 
            |> fun sb -> sb.ToString()


    // ************************************************************************
    // For sl-sqlite ?

    let noAdditions () = "no additions"

