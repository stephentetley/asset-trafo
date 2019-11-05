// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher


module RewriteMonad = 

    open AssetPatch.Base.Common

    type Rewriter<'a, 'src> = 
        | Rewriter of ('src -> Result<'a * 'src, ErrMsg>)

    let inline private apply1 (ma : Rewriter<'a, 'src>) 
                              (input : 'src) : Result<'a * 'src, ErrMsg>= 
        let (Rewriter f) = ma in f input


    let mreturn (x : 'a) : Rewriter<'a, 'src> = 
        Rewriter <| fun src -> Ok (x, src)

    let inline private bindM (ma : Rewriter<'a, 'src>) 
                             (f :'a -> Rewriter<'b, 'src>) : Rewriter<'b, 'src> =
        Rewriter <| fun src -> 
            match apply1 ma src with
            | Error msg -> Error msg
            | Ok (a, st1) -> apply1 (f a) st1

    let inline private zeroM () : Rewriter<'a, 'src> = 
        Rewriter <| fun _ -> Error "zeroM"


    /// "First success" 
    /// If mfirst fails tray to apply msecond
    let inline private combineM (rewrite1 : Rewriter<'a, 'src>) 
                                (rewrite2 : Rewriter<'a, 'src>) : Rewriter<'a, 'src> = 
        Rewriter <| fun src -> 
            match apply1 rewrite1 src with
            | Error msg -> apply1 rewrite2 src
            | Ok a -> Ok a


    let inline private delayM (fn:unit -> Rewriter<'a, 'src>) : Rewriter<'a, 'src> = 
        bindM (mreturn ()) fn 

    type RewriterBuilder() = 
        member self.Return x            = mreturn x
        member self.Bind (p,f)          = bindM p f
        member self.Zero ()             = zeroM ()
        member self.Combine (ma,mb)     = combineM ma mb
        member self.Delay fn            = delayM fn
        member self.ReturnFrom(ma)      = ma


    let (rewrite : RewriterBuilder) = new RewriterBuilder()


    let runRewriter (source : 'src) 
                    (action : Rewriter<'a, 'src>) : Result<'a * 'src, ErrMsg> = 
        apply1 action source


    let execRewriter (source : 'src) 
                        (action : Rewriter<'a, 'src>) : Result<'src, ErrMsg> = 
        runRewriter source action |> Result.map snd


    // ****************************************************
    // Input

    let internal getInput () : Rewriter<'src, 'src> =
        Rewriter <| fun src -> Ok (src, src)

        
    let internal setInput (source : 'src) : Rewriter<unit, 'src> =
        Rewriter <| fun _ -> Ok ((), source)


    // ****************************************************
    // Errors

    let rewriteError (msg : string) : Rewriter<'a, 'src> = 
        Rewriter <| fun _ -> Error msg


    let swapError (msg : string) 
                  (action : Rewriter<'a, 'src>) : Rewriter<'a, 'src> = 
        Rewriter <| fun src ->
            match apply1 action src with
            | Error _ -> Error msg
            | Ok a -> Ok a

    // ****************************************************
    // Monadic operations

    /// fmap 
    let fmapM (mapper : 'a -> 'b) (action : Rewriter<'a, 'src>) : Rewriter<'b, 'src> = 
        Rewriter <| fun src -> 
           match apply1 action src with
           | Error msg -> Error msg
           | Ok (a, st1) -> Ok (mapper a, st1)

    /// Operator for flipped fmap.
    let ( |>> ) (action : Rewriter<'a, 'src>) (mapper : 'a -> 'b) : Rewriter<'b, 'src> = 
        fmapM mapper action

    
    /// Operator for fmap.
    let ( <<| ) (mapper : 'a -> 'b) (action : Rewriter<'a, 'src>) : Rewriter<'b, 'src> = 
        fmapM mapper action


    /// Bind operator
    let ( >>= ) (action : Rewriter<'a, 'src>)
                (next : 'a -> Rewriter<'b, 'src>) : Rewriter<'b, 'src> =
        bindM action next

    /// Flipped Bind operator
    let ( =<< ) (next : 'a -> Rewriter<'b, 'src>)
                (action : Rewriter<'a, 'src>) : Rewriter<'b, 'src> =
        bindM action next

    let kleisliL (step1 : 'a -> Rewriter<'b, 'src>)
                 (step2 : 'b -> Rewriter<'c, 'src>)
                 (input : 'a) : Rewriter<'c, 'src> =
        rewrite {
            let! b = step1 input
            let! c = step2 b
            return c
        }

    /// Flipped kleisliL
    let kleisliR (step1 : 'b -> Rewriter<'c, 'src>)
                 (step2 : 'a -> Rewriter<'b, 'src>)
                 (input : 'a) : Rewriter<'c, 'src> =
        rewrite {
            let! b = step2 input
            let! c = step1 b
            return c
        }

    /// Operator for kleisliL
    let ( >=> ) (step1 : 'a -> Rewriter<'b, 'src>)
                (step2 : 'b -> Rewriter<'c, 'src>)
                (input : 'a) : Rewriter<'c, 'src> =
        kleisliL step1 step2 input


    /// Operator for kleisliR
    let ( <=< ) (step1 : 'b -> Rewriter<'c, 'src>)
                (step2 : 'a -> Rewriter<'b, 'src>)
                (input : 'a) : Rewriter<'c, 'src> =
        kleisliR step1 step2 input

    /// Perform two actions in sequence.
    /// Ignore the results of the second action if both succeed.
    let seqL (action1 : Rewriter<'a, 'src>) 
             (action2 : Rewriter<'b, 'src>) : Rewriter<'a,'src> =
        rewrite {
            let! a = action1
            let! b = action2
            return a
        }

    /// Perform two actions in sequence.
    /// Ignore the results of the first action if both succeed.
    let seqR (action1 : Rewriter<'a, 'src>) 
             (action2 : Rewriter<'b, 'src>) : Rewriter<'b, 'src> =
        rewrite {
            let! a = action1
            let! b = action2
            return b
        }

    /// Operator for seqL
    let ( .>> ) (action1 : Rewriter<'a, 'src>) 
                (action2 : Rewriter<'b, 'src>) : Rewriter<'a,'src> =
        seqL action1 action2

    /// Operator for seqR
    let ( >>. ) (action1 : Rewriter<'a, 'src>) 
                (action2 : Rewriter<'b, 'src>) : Rewriter<'b, 'src> =
        seqR action1 action2

    /// Left biased choice, if ``ma`` succeeds return its result, otherwise try ``mb``.
    let altM (action1 : Rewriter<'a, 'src>) 
             (action2 : Rewriter<'a, 'src>) : Rewriter<'a, 'src> =
        combineM action1 action2


    /// Haskell Applicative's (<*>)
    let apM (proc : Rewriter<'a -> 'b, 'src>) 
            (action : Rewriter<'a, 'src>) : Rewriter<'b, 'src> =
        rewrite {
            let! fn = proc
            let! a = action
            return (fn a)
        }


    // liftM (which is fmap)
    let liftM (mapper : 'a -> 'b) (action : Rewriter<'a, 'src>) : Rewriter<'b, 'src> = 
        fmapM mapper action


    let liftM2 (fn : 'a -> 'b -> 'x)
                (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>) : Rewriter<'x, 'src> =
        rewrite {
            let! a = ma
            let! b = mb
            return (fn a b)
        }

    let liftM3 (fn : 'a -> 'b -> 'c -> 'x)
                (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>)
                (mc : Rewriter<'c, 'src>) : Rewriter<'x, 'src> =
        rewrite {
            let! a = ma
            let! b = mb
            let! c = mc
            return (fn a b c)
        }

    let liftM4 (fn : 'a -> 'b -> 'c -> 'd -> 'x)
                (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>)
                (mc : Rewriter<'c, 'src>)
                (md : Rewriter<'d, 'src>) : Rewriter<'x, 'src> =
        rewrite {
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            return (fn a b c d)
        }


    let liftM5 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'x)
                (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>)
                (mc : Rewriter<'c, 'src>)
                (md : Rewriter<'d, 'src>)
                (me : Rewriter<'e, 'src>) : Rewriter<'x, 'src> =
        rewrite {
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            let! e = me
            return (fn a b c d e)
        }

    let liftM6 (fn : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x)
                (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>)
                (mc : Rewriter<'c, 'src>)
                (md : Rewriter<'d, 'src>)
                (me : Rewriter<'e, 'src>)
                (mf : Rewriter<'f, 'src>) : Rewriter<'x, 'src> =
        rewrite {
            let! a = ma
            let! b = mb
            let! c = mc
            let! d = md
            let! e = me
            let! f = mf
            return (fn a b c d e f)
        }


    let tupleM2 (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>) : Rewriter<'a * 'b, 'src> =
        liftM2 (fun a b -> (a,b)) ma mb

    let tupleM3 (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>)
                (mc : Rewriter<'c, 'src>) : Rewriter<'a * 'b * 'c, 'src> =
        liftM3 (fun a b c -> (a,b,c)) ma mb mc

    let tupleM4 (ma:Rewriter<'a, 'src>)
                (mb:Rewriter<'b, 'src>)
                (mc:Rewriter<'c, 'src>)
                (md:Rewriter<'d, 'src>) : Rewriter<'a * 'b * 'c * 'd, 'src> =
        liftM4 (fun a b c d -> (a,b,c,d)) ma mb mc md

    let tupleM5 (ma : Rewriter<'a, 'src>)
                 (mb : Rewriter<'b, 'src>)
                 (mc : Rewriter<'c, 'src>)
                 (md : Rewriter<'d, 'src>)
                 (me : Rewriter<'e, 'src>) : Rewriter<'a * 'b * 'c * 'd * 'e, 'src> =
        liftM5 (fun a b c d e -> (a,b,c,d,e)) ma mb mc md me

    let tupleM6 (ma : Rewriter<'a, 'src>)
                 (mb : Rewriter<'b, 'src>)
                 (mc : Rewriter<'c, 'src>)
                 (md : Rewriter<'d, 'src>)
                 (me : Rewriter<'e, 'src>)
                 (mf : Rewriter<'f, 'src>) : Rewriter<'a * 'b * 'c * 'd * 'e * 'f, 'src> =
        liftM6 (fun a b c d e f -> (a,b,c,d,e,f)) ma mb mc md me mf

    let pipeM2 (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>)
                (fn : 'a -> 'b -> 'x) : Rewriter<'x, 'src> =
        liftM2 fn ma mb

    let pipeM3 (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>)
                (mc : Rewriter<'c, 'src>)
                (fn : 'a -> 'b -> 'c -> 'x) : Rewriter<'x, 'src> =
        liftM3 fn ma mb mc

    let pipeM4 (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>)
                (mc : Rewriter<'c, 'src>)
                (md : Rewriter<'d, 'src>)
                (fn:'a -> 'b -> 'c -> 'd -> 'x) : Rewriter<'x, 'src> =
        liftM4 fn ma mb mc md

    let pipeM5 (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>)
                (mc : Rewriter<'c, 'src>)
                (md : Rewriter<'d, 'src>)
                (me : Rewriter<'e, 'src>)
                (fn : 'a -> 'b -> 'c -> 'd -> 'e ->'x) : Rewriter<'x, 'src> =
        liftM5 fn ma mb mc md me

    let pipeM6 (ma : Rewriter<'a, 'src>)
                (mb : Rewriter<'b, 'src>)
                (mc : Rewriter<'c, 'src>)
                (md : Rewriter<'d, 'src>)
                (me : Rewriter<'e, 'src>)
                (mf : Rewriter<'f, 'src>)
                (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x) : Rewriter<'x, 'src> =
        liftM6 fn ma mb mc md me mf

