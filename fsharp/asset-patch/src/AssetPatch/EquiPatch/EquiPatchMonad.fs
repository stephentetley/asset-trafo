// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.EquiPatch


module EquiPatchMonad =

    open AssetPatch.Base.Common

    /// FlocPatch is a Reader-State-Error monad to build trails
    /// of functional locations to build a structure.
    type PatchMonad<'a, 'env> = 
        PatchMonad of ('env -> FuncLoc list -> Result<'a * FuncLoc list, ErrMsg>)

    

    