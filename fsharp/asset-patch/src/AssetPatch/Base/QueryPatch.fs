// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base


module QueryPatch = 
    
    open AssetPatch.Base.Syntax
    open AssetPatch.Base.CompilerMonad

    type PatchQuery<'a, 'ptype> = CompilerMonad<'a, PatchFile<'ptype>>

    let askUser () : PatchQuery<string, 'any> = 
        asks (fun patchFile -> patchFile.User)

    let askDataModel () : PatchQuery<DataModel, 'any> = 
        asks (fun patchFile -> patchFile.DataModel)