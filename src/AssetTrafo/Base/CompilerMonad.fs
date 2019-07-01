// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Base


module CompilerMonad =

    type ErrMsg = string

    type Ans<'a, 'state> = Result<'a * 'state, ErrMsg> 

    /// The compiler monad "has everything": state + reader + error.
    /// It also has 'res is an immutable handle that cannot even be changed by 'local'
    type CompilerMonad<'a, 'res, 'env, 'state> = 
        | CompilerMonad of ('res -> 'env -> 'state -> Ans<'a, 'state>)

    let private apply1 (ma : CompilerMonad<'ans, 'res, 'env, 'state>)
                       (handle : 'res) 
                       (env : 'env) 
                       (state : 'state) : Ans<'ans, 'state> = 
            let (CompilerMonad f) = ma in f handle env state

    let private failM () : CompilerMonad<'ans, 'res, 'env, 'state> = 
        CompilerMonad <| fun _ _ _ -> Error "failM"

    let mreturn (x : 'ans) : CompilerMonad<'ans, 'res, 'env, 'state> =
        CompilerMonad <| fun _ _ st -> Ok (x, st)

