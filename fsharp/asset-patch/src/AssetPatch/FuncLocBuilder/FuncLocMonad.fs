// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FuncLocBuilder


module FuncLocMonad =

    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common   
    open AssetPatch.Base.Printer
    open AssetPatch.FuncLocBuilder
    open AssetPatch.FuncLocBuilder.FuncLocCommon
    open AssetPatch.FuncLocBuilder.FuncLocPatch
    open AssetPatch.FuncLocBuilder.ClassFlocPatch
    

    type Env = 
        { PathToInitialDownload : string
          }

    /// Floc = F(unctional) Loc(action)
    /// FlocMonad is a Reader-State-Error monad to build trails
    /// of functional locations to build a structure.
    type FlocMonad<'a> = 
        FlocMonad of (Env -> FuncLoc list -> Result<'a * FuncLoc list, ErrMsg>)



    let inline private apply1 (ma : FlocMonad<'a>) 
                              (env : Env)
                              (acc : FuncLoc list) : Result<'a * FuncLoc list, ErrMsg> = 
        let (FlocMonad fn) = ma in fn env acc

    let mreturn (x:'a) : FlocMonad<'a> = 
        FlocMonad (fun _ acc -> Ok (x, acc))

    let inline private bindM (ma : FlocMonad<'a>) 
                             (fn : 'a -> FlocMonad<'b>) : FlocMonad<'b> =
        FlocMonad <| fun env acc -> 
            match apply1 ma env acc with
            | Ok (a, acc1) -> apply1 (fn a) env acc1
            | Error msg -> Error msg

    let failM (msg:string) : FlocMonad<'a> = 
        FlocMonad (fun _ _ -> Error msg)
    
    let inline private altM  (ma : FlocMonad<'a>) 
                             (mb : FlocMonad<'a>) : FlocMonad<'a> = 
        FlocMonad <| fun env acc -> 
            match apply1 ma env acc with
            | Ok ans -> Ok ans
            | Error _ -> apply1 mb env acc
    
    
    let inline private delayM (fn : unit -> FlocMonad<'a>) : FlocMonad<'a> = 
        bindM (mreturn ()) fn 
    
    type FlocMonadBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Zero ()         = failM "Zero"
        member self.Combine (p,q)   = altM p q
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma

    let (flocBuilder : FlocMonadBuilder) = new FlocMonadBuilder()


    let execFlocMonad (env: Env) (action : FlocMonad<'a> ) : Result<FuncLoc list, ErrMsg> = 
        let (FlocMonad fn ) = action 
        match fn env [] with
        | Ok (_, changes) -> Ok changes /// List.sortWith compareFlocChange flocs |> Ok
        | Error msg -> Error msg


    /// Note - there is probably scope to add a phantom type layer over FlocLoc
    /// encoding whether you have a Site, Function, System, etc.
    let extend (itemCode : string) (description : string) (objType : string) (parent : FuncLoc) : FlocMonad<FuncLoc> = 
        /// At some point code will be looked up in a table of valid codes...
        FlocMonad <| fun env acc -> 
            let child : FuncLoc = 
                FuncLoc.extend itemCode description objType parent
            Ok (child, child :: acc)


    let root (flocCode : string) : FlocMonad<FuncLoc> = 
        FlocMonad <| fun env acc -> 
            match FuncLoc.getRootFromPathFile flocCode env.PathToInitialDownload with
            | Error msg -> Error msg
            | Ok root -> Ok (root, acc)


    /// Bind operator
    let ( >>= ) (ma : FlocMonad<'a>) 
                (fn : 'a -> FlocMonad<'b>) : FlocMonad<'b> = 
        bindM ma fn

    /// Flipped Bind operator
    let ( =<< ) (fn : 'a -> FlocMonad<'b>) 
                (ma : FlocMonad<'a>) : FlocMonad<'b> = 
        bindM ma fn


    let kleisliL (mf : 'a -> FlocMonad<'b>)
                 (mg : 'b -> FlocMonad<'c>)
                 (source:'a) : FlocMonad<'c> = 
        flocBuilder { 
            let! b = mf source
            let! c = mg b
            return c
        }

    /// Flipped kleisliL
    let kleisliR (mf : 'b -> FlocMonad<'c>)
                 (mg : 'a -> FlocMonad<'b>)
                 (source:'a) : FlocMonad<'c> = 
        flocBuilder { 
            let! b = mg source
            let! c = mf b
            return c
        }


    /// Operator for kleisliL
    let (>=>) (mf : 'a -> FlocMonad<'b>)
              (mg : 'b -> FlocMonad<'c>)
              (source:'a) : FlocMonad<'c> = 
        kleisliL mf mg source


    /// Operator for kleisliR
    let (<=<) (mf : 'b -> FlocMonad<'c>)
              (mg : 'a -> FlocMonad<'b>)
              (source:'a) : FlocMonad<'c> = 
        kleisliR mf mg source


    type BuildConfig = 
        { PathToFlocFile : string 
          User : string
          Timestamp : DateTime }
    
    let compilePatch (config : BuildConfig) 
                        (action : FlocMonad<'a> ) 
                        (root : string) 
                        (outputDirectory : string) : Result<unit, ErrMsg> = 
        let fmEnv = { PathToInitialDownload = config.PathToFlocFile }
        CompilerMonad.runCompiler () <|
            CompilerMonad.compile {
                let! flocs = CompilerMonad.liftResult <| execFlocMonad fmEnv action
                let! flPatch = makeFuncLocPatch config.User config.Timestamp flocs
                let out1 = filenameFuncLocs outputDirectory root
                writePatch out1 flPatch
                let! cfPatch = makeClassFlocPatch config.User config.Timestamp flocs
                let out2 = filenameClassFlocs outputDirectory root
                writePatch out2 cfPatch
                return ()
            }


