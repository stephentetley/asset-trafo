// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base



module CompilerMonad =
    
    
    open FSharp.Core

    open AssetPatch.Base.Common


    /// Floc = F(unctional) Loc(action)
    /// FlocMonad is a Reader-State-Error monad to build trails
    /// of functional locations to build a structure.
    type CompilerMonad<'a, 'env> = 
        CompilerMonad of ('env -> Result<'a, ErrMsg>)

    let inline private apply1 (ma : CompilerMonad<'a, 'env>) 
                                (env : 'env)  : Result<'a, ErrMsg> = 
        let (CompilerMonad fn) = ma in fn env

    let mreturn (x:'a) : CompilerMonad<'a, 'env> = 
        CompilerMonad <| fun _ -> Ok x

    let inline private bindM (ma : CompilerMonad<'a, 'env>) 
                             (fn : 'a -> CompilerMonad<'b, 'env>) : CompilerMonad<'b, 'env> =
        CompilerMonad <| fun env -> 
            match apply1 ma env with
            | Ok a -> apply1 (fn a) env
            | Error msg -> Error msg

    let failM (msg:string) : CompilerMonad<'a, 'env> = 
        CompilerMonad (fun _ -> Error msg)
    
    let inline private altM  (ma : CompilerMonad<'a, 'env>) 
                             (mb : CompilerMonad<'a, 'env>) : CompilerMonad<'a, 'env> = 
        CompilerMonad <| fun env -> 
            match apply1 ma env with
            | Ok ans -> Ok ans
            | Error _ -> apply1 mb env
    
    
    let inline private delayM (fn : unit -> CompilerMonad<'a, 'env>) : CompilerMonad<'a, 'env> = 
        bindM (mreturn ()) fn 
    
    type CompilerMonadBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Zero ()         = failM "Zero"
        member self.Combine (p,q)   = altM p q
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma


    let (compile : CompilerMonadBuilder) = new CompilerMonadBuilder()


    let runCompiler (env: 'env) (action : CompilerMonad<'a, 'env> ) : Result<'a, ErrMsg> = 
        apply1 action env

    // ************************************************************************
    // Usual monadic operations


    // Common operations
    let fmapM (update : 'a -> 'b) 
              (action : CompilerMonad<'a, 'env>) : CompilerMonad<'b, 'env> = 
        CompilerMonad <| fun env ->
            match apply1 action env with
            | Ok a -> Ok (update a)
            | Error msg -> Error msg
       
    /// Operator for fmap.
    let ( |>> ) (action : CompilerMonad<'a, 'env>) 
                (update : 'a -> 'b) : CompilerMonad<'b, 'env> = 
        fmapM update action

    /// Flipped fmap.
    let ( <<| ) (update : 'a -> 'b) 
                (action : CompilerMonad<'a, 'env>) : CompilerMonad<'b, 'env> = 
        fmapM update action

    /// Haskell Applicative's (<*>)
    let apM (mf : CompilerMonad<'a ->'b, 'env>) 
            (ma : CompilerMonad<'a, 'env>) : CompilerMonad<'b, 'env> = 
        compile { 
            let! fn = mf
            let! a = ma
            return (fn a) 
        }

    /// Operator for apM
    let ( <*> ) (ma : CompilerMonad<'a -> 'b, 'env>) 
                (mb : CompilerMonad<'a, 'env>) : CompilerMonad<'b, 'env> = 
        apM ma mb

    /// Bind operator
    let ( >>= ) (ma : CompilerMonad<'a, 'env>) 
                (fn : 'a -> CompilerMonad<'b, 'env>) : CompilerMonad<'b, 'env> = 
        bindM ma fn

    /// Flipped Bind operator
    let ( =<< ) (fn : 'a -> CompilerMonad<'b, 'env>) 
                (ma : CompilerMonad<'a, 'env>) : CompilerMonad<'b, 'env> = 
        bindM ma fn


    let kleisliL (mf : 'a -> CompilerMonad<'b, 'env>)
                 (mg : 'b -> CompilerMonad<'c, 'env>)
                 (source:'a) : CompilerMonad<'c, 'env> = 
        compile { 
            let! b = mf source
            let! c = mg b
            return c
        }

    /// Flipped kleisliL
    let kleisliR (mf : 'b -> CompilerMonad<'c, 'env>)
                 (mg : 'a -> CompilerMonad<'b, 'env>)
                 (source:'a) : CompilerMonad<'c, 'env> = 
        compile { 
            let! b = mg source
            let! c = mf b
            return c
        }


    /// Operator for kleisliL
    let (>=>) (mf : 'a -> CompilerMonad<'b, 'env>)
              (mg : 'b -> CompilerMonad<'c, 'env>)
              (source:'a) : CompilerMonad<'c, 'env> = 
        kleisliL mf mg source


    /// Operator for kleisliR
    let (<=<) (mf : 'b -> CompilerMonad<'c, 'env>)
              (mg : 'a -> CompilerMonad<'b, 'env>)
              (source:'a) : CompilerMonad<'c, 'env> = 
        kleisliR mf mg source

    /// Perform two actions in sequence. 
    /// Ignore the results of the second action if both succeed.
    let seqL (action1 : CompilerMonad<'a, 'env>) 
             (action2 : CompilerMonad<'b, 'env>) : CompilerMonad<'a, 'env> = 
        compile { 
            let! a = action1
            let! b = action2
            return a
        }

    /// Operator for seqL
    let (.>>) (action1 : CompilerMonad<'a, 'env>) 
                (action2 : CompilerMonad<'b, 'env>) : CompilerMonad<'a, 'env> = 
        seqL action1 action2

    /// Perform two actions in sequence. 
    /// Ignore the results of the first action if both succeed.
    let seqR (action1 : CompilerMonad<'a, 'env>) 
             (action2 : CompilerMonad<'b, 'env>) : CompilerMonad<'b, 'env> = 
        compile { 
            let! a = action1
            let! b = action2
            return b
        }

    /// Operator for seqR
    let (>>.) (action1 : CompilerMonad<'a, 'env>) 
              (action2 : CompilerMonad<'b, 'env>) : CompilerMonad<'b, 'env> = 
        seqR action1 action2


    // ************************************************************************
    // Errors

    let throwError (msg : string) : CompilerMonad<'a, 'env> = 
        CompilerMonad <| fun _ -> Error msg


    let swapError (newMessage : string) 
                  (ma : CompilerMonad<'a, 'env>) : CompilerMonad<'a, 'env> = 
        CompilerMonad <| fun env -> 
            match apply1 ma env with
            | Ok a -> Ok a
            | Error _ -> Error newMessage

    /// Operator for flip swapError
    let ( <?> ) (action : CompilerMonad<'a, 'env>) (msg : string) : CompilerMonad<'a, 'env> = 
        swapError msg action
    
    let augmentError (update : string -> string) 
                     (action : CompilerMonad<'a, 'env>) : CompilerMonad<'a, 'env> = 
        CompilerMonad <| fun env ->
            match apply1 action env with
            | Ok a -> Ok a
            | Error msg -> Error (update msg)

    // ************************************************************************
    // Alternatives and Optionals...



    let ( <|> ) (action1 : CompilerMonad<'a, 'env>)  
                (action2 : CompilerMonad<'a, 'env>) : CompilerMonad<'a, 'env> = 
        altM action1 action2

    let liftOption (opt : Option<'a>) : CompilerMonad<'a, 'env> = 
        match opt with
        | Some ans -> mreturn ans
        | None -> throwError "liftOption None"

    let liftResult (result : Result<'a, ErrMsg>) : CompilerMonad<'a, 'env> = 
        match result with
        | Ok ans -> mreturn ans
        | Error err -> throwError err

    let liftResultBy (errProjection : 'Err -> ErrMsg) 
                     (result : Result<'a, 'Err>) : CompilerMonad<'a, 'env> = 
        match result with
        | Ok ans -> mreturn ans
        | Error err -> throwError (errProjection err)


    /// Try to run a computation.
    /// On failure, recover or throw again with the handler.
    let attempt (action : CompilerMonad<'a, 'env>) 
                (handler : ErrMsg -> CompilerMonad<'a, 'env>) : CompilerMonad<'a, 'env> = 
        CompilerMonad <| fun env ->
            match apply1 action env with
            | Ok a -> Ok a
            | Error msg -> apply1 (handler msg) env

    /// Run a potentially failing action. If it succeeds the answer
    /// is wrapped in ``Some``. 
    /// If it fails trap the error and return ``None``.
    let optional (action : CompilerMonad<'a, 'env>) : CompilerMonad<'a option, 'env> = 
        attempt (action |>> Some) (fun _ -> mreturn None)

    /// Run an optional action - if it returns ``Some a`` return the 
    /// answer. If it returns ``None`` the fail.
    let getOptional (action : CompilerMonad<'a option, 'env>) : CompilerMonad<'a, 'env> = 
        compile { 
            match! action with
            | Some a -> return a
            | None -> return! throwError "getOptional - None" 
        }




    let choice (actions : CompilerMonad<'a, 'env> list) : CompilerMonad<'a, 'env> = 
        CompilerMonad <| fun env -> 
            let rec work acts cont = 
                match acts with 
                | [] -> Error "choice"
                | action1 :: rest ->
                    match apply1 action1 env with
                    | Ok a -> cont (Ok a)
                    | Error _ -> work rest cont
            work actions (fun x -> x)

    // ************************************************************************
    // liftM2 etc

    // liftM (which is fmap)
    let liftM (fn : 'a -> 'ans) 
                (action : CompilerMonad<'a, 'env>) : CompilerMonad<'ans, 'env> = 
        fmapM fn action

    let liftM2 (combine : 'a -> 'b -> 'ans) 
                (action1 : CompilerMonad<'a, 'env>) 
                (action2 : CompilerMonad<'b, 'env>) : CompilerMonad<'ans, 'env> = 
        compile { 
            let! a = action1
            let! b = action2
            return (combine a b)
        }

    let liftM3 (combine : 'a -> 'b -> 'c -> 'ans) 
                (action1 : CompilerMonad<'a, 'env>) 
                (action2 : CompilerMonad<'b, 'env>) 
                (action3 : CompilerMonad<'c, 'env>) : CompilerMonad<'ans, 'env> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            return (combine a b c)
        }

    let liftM4 (combine : 'a -> 'b -> 'c -> 'd -> 'ans) 
                (action1 : CompilerMonad<'a, 'env>) 
                (action2 : CompilerMonad<'b, 'env>) 
                (action3 : CompilerMonad<'c, 'env>) 
                (action4 : CompilerMonad<'d, 'env>) : CompilerMonad<'ans, 'env> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            return (combine a b c d)
        }


    let liftM5 (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'ans) 
                (action1 : CompilerMonad<'a, 'env>) 
                (action2 : CompilerMonad<'b, 'env>) 
                (action3 : CompilerMonad<'c, 'env>) 
                (action4 : CompilerMonad<'d, 'env>) 
                (action5 : CompilerMonad<'e, 'env>) : CompilerMonad<'ans, 'env> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            let! e = action5
            return (combine a b c d e)
        }

    let liftM6 (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'ans) 
                (action1 : CompilerMonad<'a, 'env>) 
                (action2 : CompilerMonad<'b, 'env>) 
                (action3 : CompilerMonad<'c, 'env>) 
                (action4 : CompilerMonad<'d, 'env>) 
                (action5 : CompilerMonad<'e, 'env>) 
                (action6 : CompilerMonad<'f, 'env>) : CompilerMonad<'ans, 'env> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            let! e = action5
            let! f = action6
            return (combine a b c d e f)
        }

    let tupleM2 (action1 : CompilerMonad<'a, 'env>) 
                (action2 : CompilerMonad<'b, 'env>) : CompilerMonad<'a * 'b, 'env> = 
        liftM2 (fun a b -> (a,b)) action1 action2

    let tupleM3 (action1 : CompilerMonad<'a, 'env>) 
                (action2 : CompilerMonad<'b, 'env>) 
                (action3 : CompilerMonad<'c, 'env>) : CompilerMonad<'a * 'b * 'c, 'env> = 
        liftM3 (fun a b c -> (a,b,c)) action1 action2 action3

    let tupleM4 (action1 : CompilerMonad<'a, 'env>) 
                (action2 : CompilerMonad<'b, 'env>) 
                (action3 : CompilerMonad<'c, 'env>) 
                (action4 : CompilerMonad<'d, 'env>) : CompilerMonad<'a * 'b * 'c * 'd, 'env> = 
        liftM4 (fun a b c d -> (a,b,c,d)) action1 action2 action3 action4

    let tupleM5 (action1 : CompilerMonad<'a, 'env>) 
                (action2 : CompilerMonad<'b, 'env>) 
                (action3 : CompilerMonad<'c, 'env>) 
                (action4 : CompilerMonad<'d, 'env>) 
                (action5 : CompilerMonad<'e, 'env>) : CompilerMonad<'a * 'b * 'c * 'd * 'e, 'env> = 
        liftM5 (fun a b c d e -> (a,b,c,d,e)) action1 action2 action3 action4 action5

    let tupleM6 (action1 : CompilerMonad<'a, 'env>) 
                (action2 : CompilerMonad<'b, 'env>) 
                (action3 : CompilerMonad<'c, 'env>) 
                (action4 : CompilerMonad<'d, 'env>) 
                (action5 : CompilerMonad<'e, 'env>) 
                (action6 : CompilerMonad<'f, 'env>) : CompilerMonad<'a * 'b * 'c * 'd * 'e * 'f, 'env> = 
        liftM6 (fun a b c d e f -> (a,b,c,d,e,f)) 
                action1 action2 action3 action4 action5 action6



    let pipeM2 (action1 : CompilerMonad<'a, 'env>) 
               (action2 : CompilerMonad<'b, 'env>) 
               (combine:'a -> 'b -> 'ans) : CompilerMonad<'ans, 'env> = 
        liftM2 combine action1 action2

    let pipeM3 (action1 : CompilerMonad<'a, 'env>) 
               (action2 : CompilerMonad<'b, 'env>) 
               (action3 : CompilerMonad<'c, 'env>) 
               (combine : 'a -> 'b -> 'c -> 'ans) : CompilerMonad<'ans, 'env> = 
        liftM3 combine action1 action2 action3

    let pipeM4 (action1 : CompilerMonad<'a, 'env>) 
               (action2 : CompilerMonad<'b, 'env>) 
               (action3 : CompilerMonad<'c, 'env>) 
               (action4 : CompilerMonad<'d, 'env>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'ans) : CompilerMonad<'ans, 'env> = 
        liftM4 combine action1 action2 action3 action4

    let pipeM5 (action1 : CompilerMonad<'a, 'env>) 
               (action2 : CompilerMonad<'b, 'env>) 
               (action3 : CompilerMonad<'c, 'env>) 
               (action4 : CompilerMonad<'d, 'env>) 
               (action5 : CompilerMonad<'e, 'env>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'ans) : CompilerMonad<'ans, 'env> = 
        liftM5 combine action1 action2 action3 action4 action5

    let pipeM6 (action1 : CompilerMonad<'a, 'env>) 
               (action2 : CompilerMonad<'b, 'env>) 
               (action3 : CompilerMonad<'c, 'env>) 
               (action4 : CompilerMonad<'d, 'env>) 
               (action5 : CompilerMonad<'e, 'env>) 
               (action6 : CompilerMonad<'f, 'env>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'ans) : CompilerMonad<'ans, 'env> = 
        liftM6 combine action1 action2 action3 action4 action5 action6

    // ************************************************************************
    // Traversals

    /// Implemented in CPS 
    let mapM (mf: 'a -> CompilerMonad<'b, 'env>) 
             (source : 'a list) : CompilerMonad<'b list, 'env> = 
        CompilerMonad <| fun env -> 
            let rec work (xs : 'a list) 
                         (fk : ErrMsg -> Result<'b list, ErrMsg>) 
                         (sk : 'b list -> Result<'b list, ErrMsg>) = 
                match xs with
                | [] -> sk []
                | y :: ys -> 
                    match apply1 (mf y) env with
                    | Error msg -> fk msg
                    | Ok a1 -> 
                        work ys fk (fun acc ->
                        sk (a1::acc))
            work source (fun msg -> Error msg) (fun ans -> Ok ans)

    let forM (xs : 'a list) 
             (fn : 'a -> CompilerMonad<'b, 'env>) : CompilerMonad<'b list, 'env> = 
        mapM fn xs


    /// Implemented in CPS 
    let mapMz (mf: 'a -> CompilerMonad<'b, 'env>) 
              (source : 'a list) : CompilerMonad<unit, 'env> = 
        CompilerMonad <| fun env -> 
            let rec work (xs : 'a list) 
                         (fk : ErrMsg -> Result<unit, ErrMsg>) 
                         (sk : unit -> Result<unit, ErrMsg>) = 
                match xs with
                | [] -> sk ()
                | y :: ys -> 
                    match apply1 (mf y) env with
                    | Error msg -> fk msg
                    | Ok _ -> 
                        work ys fk (fun acc ->
                        sk acc)
            work source (fun msg -> Error msg) (fun ans -> Ok ans)


    let forMz (xs : 'a list) 
              (fn : 'a -> CompilerMonad<'b, 'env>) : CompilerMonad<unit, 'env> = 
        mapMz fn xs

    let foldM (action : 'state -> 'a -> CompilerMonad<'state, 'env>) 
                (state : 'state)
                (source : 'a list) : CompilerMonad<'state, 'env> = 
        CompilerMonad <| fun env -> 
            let rec work (st : 'state) 
                            (xs : 'a list) 
                            (fk : ErrMsg -> Result<'state, ErrMsg>) 
                            (sk : 'state -> Result<'state, ErrMsg>) = 
                match xs with
                | [] -> sk st
                | x1 :: rest -> 
                    match apply1 (action st x1) env with
                    | Error msg -> fk msg
                    | Ok st1 -> 
                        work st1 rest fk (fun acc ->
                        sk acc)
            work state source (fun msg -> Error msg) (fun ans -> Ok ans)



    let smapM (action : 'a -> CompilerMonad<'b, 'env>) 
                (source : seq<'a>) : CompilerMonad<seq<'b>, 'env> = 
        CompilerMonad <| fun env ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (fk : ErrMsg -> Result<seq<'b>, ErrMsg>) 
                            (sk : seq<'b> -> Result<seq<'b>, ErrMsg>) = 
                if not (sourceEnumerator.MoveNext()) then 
                    sk Seq.empty
                else
                    let a1 = sourceEnumerator.Current
                    match apply1 (action a1) env with
                    | Error msg -> fk msg
                    | Ok b1 -> 
                        work fk (fun sx -> 
                        sk (seq { yield b1; yield! sx }))
            work (fun msg -> Error msg) (fun ans -> Ok ans)

    let sforM (sx : seq<'a>) 
              (fn : 'a -> CompilerMonad<'b, 'env>) : CompilerMonad<seq<'b>, 'env> = 
        smapM fn sx
    
    let smapMz (action : 'a -> CompilerMonad<'b, 'env>) 
                (source : seq<'a>) : CompilerMonad<unit, 'env> = 
        CompilerMonad <| fun env ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (fk : ErrMsg -> Result<unit, ErrMsg>) 
                            (sk : unit -> Result<unit, ErrMsg>) = 
                if not (sourceEnumerator.MoveNext()) then 
                    sk ()
                else
                    let a1 = sourceEnumerator.Current
                    match apply1 (action a1) env with
                    | Error msg -> fk msg
                    | Ok _ -> 
                        work fk sk
            work (fun msg -> Error msg) (fun ans -> Ok ans)

    
    let sforMz (source : seq<'a>) 
                (action : 'a -> CompilerMonad<'b, 'env>) : CompilerMonad<unit, 'env> = 
        smapMz action source

        
    let sfoldM (action : 'state -> 'a -> CompilerMonad<'state, 'env>) 
                (state : 'state)
                (source : seq<'a>) : CompilerMonad<'state, 'env> = 
        CompilerMonad <| fun env ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (st : 'state) 
                            (fk : ErrMsg -> Result<'state, ErrMsg>) 
                            (sk : 'state -> Result<'state, ErrMsg>) = 
                if not (sourceEnumerator.MoveNext()) then 
                    sk st
                else
                    let x1 = sourceEnumerator.Current
                    match apply1 (action st x1) env with
                    | Error msg -> fk msg
                    | Ok st1 -> 
                        work st1 fk sk
            work state (fun msg -> Error msg) (fun ans -> Ok ans)


    /// Implemented in CPS 
    let mapiM (mf : int -> 'a -> CompilerMonad<'b, 'env>) 
                (source : 'a list) : CompilerMonad<'b list, 'env> = 
        CompilerMonad <| fun env -> 
            let rec work (xs : 'a list)
                         (count : int)
                         (fk : ErrMsg -> Result<'b list, ErrMsg>) 
                         (sk : 'b list -> Result<'b list, ErrMsg>) = 
                match xs with
                | [] -> sk []
                | y :: ys -> 
                    match apply1 (mf count y) env with
                    | Error msg -> fk msg
                    | Ok a1 -> 
                        work ys (count+1) fk (fun acc ->
                        sk (a1::acc))
            work source 0 (fun msg -> Error msg) (fun ans -> Ok ans)


    /// Implemented in CPS 
    let mapiMz (mf : int -> 'a -> CompilerMonad<'b, 'env>) 
                (source : 'a list) : CompilerMonad<unit, 'env> = 
        CompilerMonad <| fun env -> 
            let rec work (xs : 'a list) 
                         (count : int)
                         (fk : ErrMsg -> Result<unit, ErrMsg>) 
                         (sk : unit -> Result<unit, ErrMsg>) = 
                match xs with
                | [] -> sk ()
                | y :: ys -> 
                    match apply1 (mf count y) env with
                    | Error msg -> fk msg
                    | Ok _ -> 
                        work ys (count+1) fk sk
            work source 0 (fun msg -> Error msg) (fun ans -> Ok ans)

    

    let foriM (xs : 'a list) 
              (fn : int -> 'a -> CompilerMonad<'b, 'env>) : CompilerMonad<'b list, 'env> = 
        mapiM fn xs

    let foriMz (xs : 'a list) 
               (fn : int -> 'a -> CompilerMonad<'b, 'env>) : CompilerMonad<unit, 'env> = 
        mapiMz fn xs


