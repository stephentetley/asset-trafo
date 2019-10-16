﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangeRequest


module FlocMonad =

    open AssetSync.ChangeRequest.Syntax

    type ErrMsg = string

    // writer & failure (maybe handle)

    /// Floc = F(unctional) Loc(action)
    /// FlocMonad is a State-Error monad to build trails
    /// of functional locations to build a structure.
    type FlocMonad<'a> = 
        FlocMonad of (ChangeRequest list -> Result<'a * ChangeRequest list, ErrMsg>)



    let inline private apply1 (ma : FlocMonad<'a>) 
                              (acc : ChangeRequest list) : Result<'a * ChangeRequest list, ErrMsg> = 
        let (FlocMonad fn) = ma in fn acc

    let mreturn (x:'a) : FlocMonad<'a> = 
        FlocMonad (fun acc -> Ok (x, acc))

    let inline private bindM (ma : FlocMonad<'a>) 
                             (fn : 'a -> FlocMonad<'b>) : FlocMonad<'b> =
        FlocMonad <| fun acc -> 
            match apply1 ma acc with
            | Ok (a, acc1) -> apply1 (fn a) acc1
            | Error msg -> Error msg

    let failM (msg:string) : FlocMonad<'a> = 
        FlocMonad (fun _ -> Error msg)
    
    let inline private altM  (ma : FlocMonad<'a>) 
                             (mb : FlocMonad<'a>) : FlocMonad<'a> = 
        FlocMonad <| fun acc -> 
            match apply1 ma acc with
            | Ok ans -> Ok ans
            | Error _ -> apply1 mb acc
    
    
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


    let execFlocMonad (action : FlocMonad<'a> ) : Result<ChangeRequest list, ErrMsg> = 
        let (FlocMonad fn ) = action 
        match fn [] with
        | Ok (_, changes) -> Ok changes /// List.sortWith compareFlocChange flocs |> Ok
        | Error msg -> Error msg



    let extend (code : string) (parent : FlocRequest) : FlocMonad<FlocRequest> = 
        /// At some point code will be looked up in a table of valid codes...
        FlocMonad <| fun acc -> 
            let child : FlocRequest = 
                { Name = None
                  Path = parent.Path.Extend code }
            Ok (child, FlocRequest child :: acc)


    let addEquipment (name : string) (code : uint64) (parent : FlocRequest) : FlocMonad<EquipmentRequest> = 
        /// At some point code will be looked up in a table of valid codes...
        FlocMonad <| fun acc -> 
            let child : EquipmentRequest = 
                { Name = name
                  Code = code
                  Path = parent.Path }
            Ok (child, EquipmentRequest child :: acc)

    let root (path : string) : FlocMonad<FlocRequest> = 
        FlocMonad <| fun acc -> 
            let flocPath = FlocPath.Create path
            let node : FlocRequest = 
                { Name = None
                  Path = flocPath
                }
            Ok (node, acc)

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