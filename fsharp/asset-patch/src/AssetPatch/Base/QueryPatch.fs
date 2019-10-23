// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base


module QueryPatch = 
    
    open AssetPatch.Base.Syntax
    open AssetPatch.Base.CompilerMonad

    type PatchQuery<'a, 'patchType> = CompilerMonad<'a, PatchFile<'patchType>>

    let askUser () : PatchQuery<string, 'any> = 
        asks (fun patchFile -> patchFile.Header.User)

    let askDataModel () : PatchQuery<DataModel, 'any> = 
        asks (fun patchFile -> patchFile.Header.DataModel)

    let askEntityType () : PatchQuery<EntityType, 'any> = 
        asks (fun patchFile -> patchFile.Header.EntityType)