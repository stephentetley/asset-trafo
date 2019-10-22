// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FuncLocBuilder



module CompilerMonad =
    
    
    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.Syntax

    // To fill out...
    type Env = Unit

    /// Floc = F(unctional) Loc(action)
    /// FlocMonad is a Reader-State-Error monad to build trails
    /// of functional locations to build a structure.
    type CompilerMonad<'a> = 
        CompilerMonad of (Env -> Result<'a, ErrMsg>)

    let inline private apply1 (ma : CompilerMonad<'a>) 
                                (env : Env)  : Result<'a, ErrMsg> = 
        let (CompilerMonad fn) = ma in fn env

    let mreturn (x:'a) : CompilerMonad<'a> = 
        CompilerMonad <| fun _ -> Ok x

    let inline private bindM (ma : CompilerMonad<'a>) 
                             (fn : 'a -> CompilerMonad<'b>) : CompilerMonad<'b> =
        CompilerMonad <| fun env -> 
            match apply1 ma env with
            | Ok a -> apply1 (fn a) env
            | Error msg -> Error msg

    let failM (msg:string) : CompilerMonad<'a> = 
        CompilerMonad (fun _ -> Error msg)
    
    let inline private altM  (ma : CompilerMonad<'a>) 
                             (mb : CompilerMonad<'a>) : CompilerMonad<'a> = 
        CompilerMonad <| fun env -> 
            match apply1 ma env with
            | Ok ans -> Ok ans
            | Error _ -> apply1 mb env
    
    
    let inline private delayM (fn : unit -> CompilerMonad<'a>) : CompilerMonad<'a> = 
        bindM (mreturn ()) fn 
    
    type CompilerMonadBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Zero ()         = failM "Zero"
        member self.Combine (p,q)   = altM p q
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma

    let (compile : CompilerMonadBuilder) = new CompilerMonadBuilder()


    let runFlocMonad (env: Env) (action : CompilerMonad<'a> ) : Result<'a, ErrMsg> = 
        apply1 action env


    /// Bind operator
    let ( >>= ) (ma : CompilerMonad<'a>) 
                (fn : 'a -> CompilerMonad<'b>) : CompilerMonad<'b> = 
        bindM ma fn

    /// Flipped Bind operator
    let ( =<< ) (fn : 'a -> CompilerMonad<'b>) 
                (ma : CompilerMonad<'a>) : CompilerMonad<'b> = 
        bindM ma fn


    let kleisliL (mf : 'a -> CompilerMonad<'b>)
                 (mg : 'b -> CompilerMonad<'c>)
                 (source:'a) : CompilerMonad<'c> = 
        compile { 
            let! b = mf source
            let! c = mg b
            return c
        }

    /// Flipped kleisliL
    let kleisliR (mf : 'b -> CompilerMonad<'c>)
                 (mg : 'a -> CompilerMonad<'b>)
                 (source:'a) : CompilerMonad<'c> = 
        compile { 
            let! b = mg source
            let! c = mf b
            return c
        }


    /// Operator for kleisliL
    let (>=>) (mf : 'a -> CompilerMonad<'b>)
              (mg : 'b -> CompilerMonad<'c>)
              (source:'a) : CompilerMonad<'c> = 
        kleisliL mf mg source


    /// Operator for kleisliR
    let (<=<) (mf : 'b -> CompilerMonad<'c>)
              (mg : 'a -> CompilerMonad<'b>)
              (source:'a) : CompilerMonad<'c> = 
        kleisliR mf mg source