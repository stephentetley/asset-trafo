// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.Base


module Addendum =


    open SLSqlite.Core

    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)

    // ************************************************************************
    // For sl-sqlite ?

    /// Added to sl-sqlite after last build
    type ErrorAugmentFormat = Printf.StringFormat<string -> string,string>
    
    /// Version in current build is wrong ( |?%>> ) , use this alternative 
    /// until the next build then do renaming...
    let ( |?%>>> ) (action : SqliteDb<'a>) 
                   (errorModifier : ErrorAugmentFormat) : SqliteDb<'a> = 
        augmentErrorFmt errorModifier action


