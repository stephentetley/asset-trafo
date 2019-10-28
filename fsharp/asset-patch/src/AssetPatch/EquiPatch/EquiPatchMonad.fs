// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.EquiPatch


module EquiPatchMonad =

    open AssetPatch.Base.Common
    open AssetPatch.Base.AbsChangeFile

    /// PatchMonad is a Reader-State-Error monad to build patches - lists of 
    /// change files.
    type PatchMonad<'a, 'env> = 
        PatchMonad of ('env -> AbsChangeFile list -> Result<'a * AbsChangeFile list, ErrMsg>)

    

    