// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocPatch


module FlocPatchMonad =

    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common   
    open AssetPatch.Base.Printer
    open AssetPatch.FlocPatch
    open AssetPatch.FlocPatch.Common
    open AssetPatch.FlocPatch.FunctionalLocation
    open AssetPatch.FlocPatch.FuncLocPatch
    open AssetPatch.FlocPatch.ClassFlocPatch
    

    type Env = 
        { PathToFuncLocDownload : string
          }

    /// Floc = F(unctional) Loc(action)
    /// FlocPatch is a Reader-State-Error monad to build patches - change files.
    type FlocPatch<'a> = 
        FlocPatch of (Env -> FunctionalLocation list -> Result<'a * FunctionalLocation list, ErrMsg>)



    let inline private apply1 (ma : FlocPatch<'a>) 
                              (env : Env)
                              (acc : FunctionalLocation list) : Result<'a * FunctionalLocation list, ErrMsg> = 
        let (FlocPatch fn) = ma in fn env acc

    let mreturn (x:'a) : FlocPatch<'a> = 
        FlocPatch (fun _ acc -> Ok (x, acc))

    let inline private bindM (ma : FlocPatch<'a>) 
                             (fn : 'a -> FlocPatch<'b>) : FlocPatch<'b> =
        FlocPatch <| fun env acc -> 
            match apply1 ma env acc with
            | Ok (a, acc1) -> apply1 (fn a) env acc1
            | Error msg -> Error msg

    let failM (msg:string) : FlocPatch<'a> = 
        FlocPatch (fun _ _ -> Error msg)
    
    let inline private altM  (ma : FlocPatch<'a>) 
                             (mb : FlocPatch<'a>) : FlocPatch<'a> = 
        FlocPatch <| fun env acc -> 
            match apply1 ma env acc with
            | Ok ans -> Ok ans
            | Error _ -> apply1 mb env acc
    
    
    let inline private delayM (fn : unit -> FlocPatch<'a>) : FlocPatch<'a> = 
        bindM (mreturn ()) fn 
    
    type FlocPatchBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Zero ()         = failM "Zero"
        member self.Combine (p,q)   = altM p q
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma

    let (flocpatch : FlocPatchBuilder) = new FlocPatchBuilder()

    let runFlocPatch (env: Env) (action : FlocPatch<'a> ) : Result<'a * FunctionalLocation list, ErrMsg> = 
        let (FlocPatch fn ) = action 
        match fn env [] with
        | Ok ans -> Ok ans
        | Error msg -> Error msg

    let execFlocPatch (env: Env) (action : FlocPatch<'a> ) : Result<FunctionalLocation list, ErrMsg> = 
        let (FlocPatch fn ) = action 
        match fn env [] with
        | Ok (_, changes) -> Ok changes /// List.sortWith compareFlocChange flocs |> Ok
        | Error msg -> Error msg


    /// Note - there is probably scope to add a phantom type layer over FlocLoc
    /// encoding whether you have a Site, Function, System, etc.
    let extend (itemCode : string) 
                (description : string) 
                (objType : string) 
                (parent : FunctionalLocation) : FlocPatch<FunctionalLocation> = 
        /// At some point code will be looked up in a table of valid codes...
        FlocPatch <| fun env acc -> 
            let child : FunctionalLocation = 
                FunctionalLocation.extend itemCode description objType parent
            Ok (child, child :: acc)


    let root (flocCode : string) : FlocPatch<FunctionalLocation> = 
        FlocPatch <| fun env acc -> 
            match FunctionalLocation.getRootFromPathFile flocCode env.PathToFuncLocDownload with
            | Error msg -> Error msg
            | Ok root -> Ok (root, acc)


    /// Bind operator
    let ( >>= ) (ma : FlocPatch<'a>) 
                (fn : 'a -> FlocPatch<'b>) : FlocPatch<'b> = 
        bindM ma fn

    /// Flipped Bind operator
    let ( =<< ) (fn : 'a -> FlocPatch<'b>) 
                (ma : FlocPatch<'a>) : FlocPatch<'b> = 
        bindM ma fn


    let kleisliL (mf : 'a -> FlocPatch<'b>)
                 (mg : 'b -> FlocPatch<'c>)
                 (source:'a) : FlocPatch<'c> = 
        flocpatch { 
            let! b = mf source
            let! c = mg b
            return c
        }

    /// Flipped kleisliL
    let kleisliR (mf : 'b -> FlocPatch<'c>)
                 (mg : 'a -> FlocPatch<'b>)
                 (source:'a) : FlocPatch<'c> = 
        flocpatch { 
            let! b = mg source
            let! c = mf b
            return c
        }


    /// Operator for kleisliL
    let (>=>) (mf : 'a -> FlocPatch<'b>)
              (mg : 'b -> FlocPatch<'c>)
              (source:'a) : FlocPatch<'c> = 
        kleisliL mf mg source


    /// Operator for kleisliR
    let (<=<) (mf : 'b -> FlocPatch<'c>)
              (mg : 'a -> FlocPatch<'b>)
              (source:'a) : FlocPatch<'c> = 
        kleisliR mf mg source


    type BuildConfig = 
        { PathToFlocFile : string 
          User : string
          Timestamp : DateTime }
    
    let compilePatch (config : BuildConfig) 
                        (action : FlocPatch<'a> ) 
                        (root : string) 
                        (outputDirectory : string) : Result<'a, ErrMsg> = 
        let fmEnv = { PathToFuncLocDownload = config.PathToFlocFile }
        CompilerMonad.runCompiler () <|
            CompilerMonad.compile {
                let! (ans, flocs) = 
                    CompilerMonad.liftResult <| runFlocPatch fmEnv action
                let! flPatch = makeFuncLocPatch config.User config.Timestamp flocs
                let out1 = filenameFuncLocs outputDirectory root
                writePatchAndMetadata out1 flPatch
                let! cfPatch = makeClassFlocPatch config.User config.Timestamp flocs
                let out2 = filenameClassFlocs outputDirectory root
                writePatchAndMetadata out2 cfPatch
                return ans
            }


