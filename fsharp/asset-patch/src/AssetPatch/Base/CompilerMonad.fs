// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base



module CompilerMonad =
    
    
    open FSharp.Core

    open AssetPatch.Base.Common


    /// CompilerMonad is a Reader-Accumulator(state)-Error monad.
    type CompilerMonad<'a, 'env, 'acc> = 
        CompilerMonad of ('env -> 'acc -> Result<'a * 'acc, ErrMsg>)

    let inline private apply1 (ma : CompilerMonad<'a, 'env, 'acc>) 
                                (env : 'env) (acc : 'acc) : Result<'a * 'acc, ErrMsg> = 
        let (CompilerMonad fn) = ma in fn env acc

    let mreturn (x:'a) : CompilerMonad<'a, 'env, 'acc> = 
        CompilerMonad <| fun _ ac -> Ok (x, ac)

    let inline private bindM (ma : CompilerMonad<'a, 'env, 'acc>) 
                             (fn : 'a -> CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<'b, 'env, 'acc> =
        CompilerMonad <| fun env ac -> 
            match apply1 ma env ac with
            | Ok (a, ac1) -> apply1 (fn a) env ac1
            | Error msg -> Error msg

    let failM (msg:string) : CompilerMonad<'a, 'env, 'acc> = 
        CompilerMonad (fun _ _ -> Error msg)
    
    let inline private altM  (ma : CompilerMonad<'a, 'env, 'acc>) 
                             (mb : CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'a, 'env, 'acc> = 
        CompilerMonad <| fun env ac -> 
            match apply1 ma env ac with
            | Ok ans -> Ok ans
            | Error _ -> apply1 mb env ac
    
    
    let inline private delayM (fn : unit -> CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'a, 'env, 'acc> = 
        bindM (mreturn ()) fn 
    
    type CompilerMonadBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Zero ()         = failM "Zero"
        member self.Combine (p,q)   = altM p q
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma


    let (compile : CompilerMonadBuilder) = new CompilerMonadBuilder()


    let runCompiler (env: 'env) 
                    (accZero: 'acc) 
                    (action : CompilerMonad<'a, 'env, 'acc> ) : Result<'a * 'acc, ErrMsg> = 
        apply1 action env accZero

    let evalCompiler (env: 'env) 
                        (accZero: 'acc) 
                        (action : CompilerMonad<'a, 'env, 'acc> ) : Result<'a, ErrMsg> = 
        apply1 action env accZero |> Result.map fst

    let execCompiler (env: 'env) 
                        (accZero: 'acc) 
                        (action : CompilerMonad<'a, 'env, 'acc> ) : Result<'acc, ErrMsg> = 
        apply1 action env accZero |> Result.map snd

    // ************************************************************************
    // Usual monadic operations


    // Common operations
    let fmapM (update : 'a -> 'b) 
              (action : CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'b, 'env, 'acc> = 
        CompilerMonad <| fun env ac ->
            match apply1 action env ac with
            | Ok (a, ac1) -> Ok (update a, ac1)
            | Error msg -> Error msg
       
    /// Operator for fmap.
    let ( |>> ) (action : CompilerMonad<'a, 'env, 'acc>) 
                (update : 'a -> 'b) : CompilerMonad<'b, 'env, 'acc> = 
        fmapM update action

    /// Flipped fmap.
    let ( <<| ) (update : 'a -> 'b) 
                (action : CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'b, 'env, 'acc> = 
        fmapM update action

    /// Haskell Applicative's (<*>)
    let apM (mf : CompilerMonad<'a ->'b, 'env, 'acc>) 
            (ma : CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'b, 'env, 'acc> = 
        compile { 
            let! fn = mf
            let! a = ma
            return (fn a) 
        }

    /// Operator for apM
    let ( <*> ) (ma : CompilerMonad<'a -> 'b, 'env, 'acc>) 
                (mb : CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'b, 'env, 'acc> = 
        apM ma mb

    /// Bind operator
    let ( >>= ) (ma : CompilerMonad<'a, 'env, 'acc>) 
                (fn : 'a -> CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<'b, 'env, 'acc> = 
        bindM ma fn

    /// Flipped Bind operator
    let ( =<< ) (fn : 'a -> CompilerMonad<'b, 'env, 'acc>) 
                (ma : CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'b, 'env, 'acc> = 
        bindM ma fn


    let kleisliL (mf : 'a -> CompilerMonad<'b, 'env, 'acc>)
                 (mg : 'b -> CompilerMonad<'c, 'env, 'acc>)
                 (source:'a) : CompilerMonad<'c, 'env, 'acc> = 
        compile { 
            let! b = mf source
            let! c = mg b
            return c
        }

    /// Flipped kleisliL
    let kleisliR (mf : 'b -> CompilerMonad<'c, 'env, 'acc>)
                 (mg : 'a -> CompilerMonad<'b, 'env, 'acc>)
                 (source:'a) : CompilerMonad<'c, 'env, 'acc> = 
        compile { 
            let! b = mg source
            let! c = mf b
            return c
        }


    /// Operator for kleisliL
    let (>=>) (mf : 'a -> CompilerMonad<'b, 'env, 'acc>)
              (mg : 'b -> CompilerMonad<'c, 'env, 'acc>)
              (source:'a) : CompilerMonad<'c, 'env, 'acc> = 
        kleisliL mf mg source


    /// Operator for kleisliR
    let (<=<) (mf : 'b -> CompilerMonad<'c, 'env, 'acc>)
              (mg : 'a -> CompilerMonad<'b, 'env, 'acc>)
              (source:'a) : CompilerMonad<'c, 'env, 'acc> = 
        kleisliR mf mg source

    /// Perform two actions in sequence. 
    /// Ignore the results of the second action if both succeed.
    let seqL (action1 : CompilerMonad<'a, 'env, 'acc>) 
             (action2 : CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<'a, 'env, 'acc> = 
        compile { 
            let! a = action1
            let! b = action2
            return a
        }

    /// Operator for seqL
    let (.>>) (action1 : CompilerMonad<'a, 'env, 'acc>) 
                (action2 : CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<'a, 'env, 'acc> = 
        seqL action1 action2

    /// Perform two actions in sequence. 
    /// Ignore the results of the first action if both succeed.
    let seqR (action1 : CompilerMonad<'a, 'env, 'acc>) 
             (action2 : CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<'b, 'env, 'acc> = 
        compile { 
            let! a = action1
            let! b = action2
            return b
        }

    /// Operator for seqR
    let (>>.) (action1 : CompilerMonad<'a, 'env, 'acc>) 
              (action2 : CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<'b, 'env, 'acc> = 
        seqR action1 action2


    // ************************************************************************
    // Errors

    let throwError (msg : string) : CompilerMonad<'a, 'env, 'acc> = 
        CompilerMonad <| fun _ _ -> Error msg


    let swapError (newMessage : string) 
                  (ma : CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'a, 'env, 'acc> = 
        CompilerMonad <| fun env ac -> 
            match apply1 ma env ac with
            | Ok ans -> Ok ans
            | Error _ -> Error newMessage

    /// Operator for flip swapError
    let ( <?> ) (action : CompilerMonad<'a, 'env, 'acc>) (msg : string) : CompilerMonad<'a, 'env, 'acc> = 
        swapError msg action
    
    let augmentError (update : string -> string) 
                     (action : CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'a, 'env, 'acc> = 
        CompilerMonad <| fun env ac->
            match apply1 action env ac with
            | Ok ans -> Ok ans
            | Error msg -> Error (update msg)


    // ************************************************************************
    // Reader operations

    let ask () : CompilerMonad<'env, 'env, 'acc> = 
        CompilerMonad <| fun env ac -> Ok (env, ac)

    let asks (projection : 'env -> 'ans) : CompilerMonad<'ans, 'env, 'acc> = 
        CompilerMonad <| fun env ac -> Ok (projection env, ac)




    // ************************************************************************
    // Alternatives and Optionals...



    let ( <|> ) (action1 : CompilerMonad<'a, 'env, 'acc>)  
                (action2 : CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'a, 'env, 'acc> = 
        altM action1 action2

    let liftOption (opt : Option<'a>) : CompilerMonad<'a, 'env, 'acc> = 
        match opt with
        | Some ans -> mreturn ans
        | None -> throwError "liftOption None"

    let liftResult (result : Result<'a, ErrMsg>) : CompilerMonad<'a, 'env, 'acc> = 
        match result with
        | Ok ans -> mreturn ans
        | Error err -> throwError err

    let liftResultBy (errProjection : 'Err -> ErrMsg) 
                     (result : Result<'a, 'Err>) : CompilerMonad<'a, 'env, 'acc> = 
        match result with
        | Ok ans -> mreturn ans
        | Error err -> throwError (errProjection err)


    /// Try to run a computation.
    /// On failure, recover or throw again with the handler.
    let attempt (action : CompilerMonad<'a, 'env, 'acc>) 
                (handler : ErrMsg -> CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'a, 'env, 'acc> = 
        CompilerMonad <| fun env ac ->
            match apply1 action env ac with
            | Ok ans -> Ok ans
            | Error msg -> apply1 (handler msg) env ac

    /// Run a potentially failing action. If it succeeds the answer
    /// is wrapped in ``Some``. 
    /// If it fails trap the error and return ``None``.
    let optional (action : CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'a option, 'env, 'acc> = 
        attempt (action |>> Some) (fun _ -> mreturn None)

    /// Run an optional action - if it returns ``Some a`` return the 
    /// answer. If it returns ``None`` the fail.
    let getOptional (action : CompilerMonad<'a option, 'env, 'acc>) : CompilerMonad<'a, 'env, 'acc> = 
        compile { 
            match! action with
            | Some a -> return a
            | None -> return! throwError "getOptional - None" 
        }




    let choice (actions : CompilerMonad<'a, 'env, 'acc> list) : CompilerMonad<'a, 'env, 'acc> = 
        CompilerMonad <| fun env accum -> 
            let rec work acts ac cont = 
                match acts with 
                | [] -> Error "choice"
                | action1 :: rest ->
                    match apply1 action1 env ac with
                    | Ok ans -> cont (Ok ans)
                    | Error _ -> work rest ac cont
            work actions accum (fun x -> x)

    // ************************************************************************
    // liftM2 etc

    // liftM (which is fmap)
    let liftM (fn : 'a -> 'ans) 
                (action : CompilerMonad<'a, 'env, 'acc>) : CompilerMonad<'ans, 'env, 'acc> = 
        fmapM fn action

    let liftM2 (combine : 'a -> 'b -> 'ans) 
                (action1 : CompilerMonad<'a, 'env, 'acc>) 
                (action2 : CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<'ans, 'env, 'acc> = 
        compile { 
            let! a = action1
            let! b = action2
            return (combine a b)
        }

    let liftM3 (combine : 'a -> 'b -> 'c -> 'ans) 
                (action1 : CompilerMonad<'a, 'env, 'acc>) 
                (action2 : CompilerMonad<'b, 'env, 'acc>) 
                (action3 : CompilerMonad<'c, 'env, 'acc>) : CompilerMonad<'ans, 'env, 'acc> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            return (combine a b c)
        }

    let liftM4 (combine : 'a -> 'b -> 'c -> 'd -> 'ans) 
                (action1 : CompilerMonad<'a, 'env, 'acc>) 
                (action2 : CompilerMonad<'b, 'env, 'acc>) 
                (action3 : CompilerMonad<'c, 'env, 'acc>) 
                (action4 : CompilerMonad<'d, 'env, 'acc>) : CompilerMonad<'ans, 'env, 'acc> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            return (combine a b c d)
        }


    let liftM5 (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'ans) 
                (action1 : CompilerMonad<'a, 'env, 'acc>) 
                (action2 : CompilerMonad<'b, 'env, 'acc>) 
                (action3 : CompilerMonad<'c, 'env, 'acc>) 
                (action4 : CompilerMonad<'d, 'env, 'acc>) 
                (action5 : CompilerMonad<'e, 'env, 'acc>) : CompilerMonad<'ans, 'env, 'acc> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            let! e = action5
            return (combine a b c d e)
        }

    let liftM6 (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'ans) 
                (action1 : CompilerMonad<'a, 'env, 'acc>) 
                (action2 : CompilerMonad<'b, 'env, 'acc>) 
                (action3 : CompilerMonad<'c, 'env, 'acc>) 
                (action4 : CompilerMonad<'d, 'env, 'acc>) 
                (action5 : CompilerMonad<'e, 'env, 'acc>) 
                (action6 : CompilerMonad<'f, 'env, 'acc>) : CompilerMonad<'ans, 'env, 'acc> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            let! e = action5
            let! f = action6
            return (combine a b c d e f)
        }

    let tupleM2 (action1 : CompilerMonad<'a, 'env, 'acc>) 
                (action2 : CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<'a * 'b, 'env, 'acc> = 
        liftM2 (fun a b -> (a,b)) action1 action2

    let tupleM3 (action1 : CompilerMonad<'a, 'env, 'acc>) 
                (action2 : CompilerMonad<'b, 'env, 'acc>) 
                (action3 : CompilerMonad<'c, 'env, 'acc>) : CompilerMonad<'a * 'b * 'c, 'env, 'acc> = 
        liftM3 (fun a b c -> (a,b,c)) action1 action2 action3

    let tupleM4 (action1 : CompilerMonad<'a, 'env, 'acc>) 
                (action2 : CompilerMonad<'b, 'env, 'acc>) 
                (action3 : CompilerMonad<'c, 'env, 'acc>) 
                (action4 : CompilerMonad<'d, 'env, 'acc>) : CompilerMonad<'a * 'b * 'c * 'd, 'env, 'acc> = 
        liftM4 (fun a b c d -> (a,b,c,d)) action1 action2 action3 action4

    let tupleM5 (action1 : CompilerMonad<'a, 'env, 'acc>) 
                (action2 : CompilerMonad<'b, 'env, 'acc>) 
                (action3 : CompilerMonad<'c, 'env, 'acc>) 
                (action4 : CompilerMonad<'d, 'env, 'acc>) 
                (action5 : CompilerMonad<'e, 'env, 'acc>) : CompilerMonad<'a * 'b * 'c * 'd * 'e, 'env, 'acc> = 
        liftM5 (fun a b c d e -> (a,b,c,d,e)) action1 action2 action3 action4 action5

    let tupleM6 (action1 : CompilerMonad<'a, 'env, 'acc>) 
                (action2 : CompilerMonad<'b, 'env, 'acc>) 
                (action3 : CompilerMonad<'c, 'env, 'acc>) 
                (action4 : CompilerMonad<'d, 'env, 'acc>) 
                (action5 : CompilerMonad<'e, 'env, 'acc>) 
                (action6 : CompilerMonad<'f, 'env, 'acc>) : CompilerMonad<'a * 'b * 'c * 'd * 'e * 'f, 'env, 'acc> = 
        liftM6 (fun a b c d e f -> (a,b,c,d,e,f)) 
                action1 action2 action3 action4 action5 action6



    let pipeM2 (action1 : CompilerMonad<'a, 'env, 'acc>) 
               (action2 : CompilerMonad<'b, 'env, 'acc>) 
               (combine:'a -> 'b -> 'ans) : CompilerMonad<'ans, 'env, 'acc> = 
        liftM2 combine action1 action2

    let pipeM3 (action1 : CompilerMonad<'a, 'env, 'acc>) 
               (action2 : CompilerMonad<'b, 'env, 'acc>) 
               (action3 : CompilerMonad<'c, 'env, 'acc>) 
               (combine : 'a -> 'b -> 'c -> 'ans) : CompilerMonad<'ans, 'env, 'acc> = 
        liftM3 combine action1 action2 action3

    let pipeM4 (action1 : CompilerMonad<'a, 'env, 'acc>) 
               (action2 : CompilerMonad<'b, 'env, 'acc>) 
               (action3 : CompilerMonad<'c, 'env, 'acc>) 
               (action4 : CompilerMonad<'d, 'env, 'acc>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'ans) : CompilerMonad<'ans, 'env, 'acc> = 
        liftM4 combine action1 action2 action3 action4

    let pipeM5 (action1 : CompilerMonad<'a, 'env, 'acc>) 
               (action2 : CompilerMonad<'b, 'env, 'acc>) 
               (action3 : CompilerMonad<'c, 'env, 'acc>) 
               (action4 : CompilerMonad<'d, 'env, 'acc>) 
               (action5 : CompilerMonad<'e, 'env, 'acc>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'ans) : CompilerMonad<'ans, 'env, 'acc> = 
        liftM5 combine action1 action2 action3 action4 action5

    let pipeM6 (action1 : CompilerMonad<'a, 'env, 'acc>) 
               (action2 : CompilerMonad<'b, 'env, 'acc>) 
               (action3 : CompilerMonad<'c, 'env, 'acc>) 
               (action4 : CompilerMonad<'d, 'env, 'acc>) 
               (action5 : CompilerMonad<'e, 'env, 'acc>) 
               (action6 : CompilerMonad<'f, 'env, 'acc>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'ans) : CompilerMonad<'ans, 'env, 'acc> = 
        liftM6 combine action1 action2 action3 action4 action5 action6

    // ************************************************************************
    // Traversals

    /// Implemented in CPS 
    let mapM (mf: 'a -> CompilerMonad<'b, 'env, 'acc>) 
             (source : 'a list) : CompilerMonad<'b list, 'env, 'acc> = 
        CompilerMonad <| fun env accum -> 
            let rec work (xs : 'a list)
                         (ac : 'acc)
                         (fk : ErrMsg -> Result<'b list * 'acc, ErrMsg>) 
                         (sk : 'b list -> 'acc -> Result<'b list * 'acc, ErrMsg>) = 
                match xs with
                | [] -> sk [] ac
                | y :: ys -> 
                    match apply1 (mf y) env ac with
                    | Error msg -> fk msg
                    | Ok (v1, ac1)  -> 
                        work ys ac1 fk (fun vs ac2 ->
                        sk (v1::vs) ac2)
            work source accum (fun msg -> Error msg) (fun ans ac -> Ok (ans, ac))

    let forM (xs : 'a list) 
             (fn : 'a -> CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<'b list, 'env, 'acc> = 
        mapM fn xs


    /// Implemented in CPS 
    let mapMz (mf: 'a -> CompilerMonad<'b, 'env, 'acc>) 
              (source : 'a list) : CompilerMonad<unit, 'env, 'acc> = 
        CompilerMonad <| fun env accum -> 
            let rec work (xs : 'a list) 
                         (ac : 'acc)
                         (fk : ErrMsg -> Result<unit * 'acc, ErrMsg>) 
                         (sk : 'acc -> Result<unit * 'acc, ErrMsg>) = 
                match xs with
                | [] -> sk ac
                | y :: ys -> 
                    match apply1 (mf y) env ac with
                    | Error msg -> fk msg
                    | Ok (_, ac1) -> 
                        work ys ac1 fk (fun ac2 ->
                        sk ac2)
            work source accum (fun msg -> Error msg) (fun ac -> Ok ((), ac))


    let forMz (xs : 'a list) 
              (fn : 'a -> CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<unit, 'env, 'acc> = 
        mapMz fn xs

    let foldM (action : 'state -> 'a -> CompilerMonad<'state, 'env, 'acc>) 
                (state : 'state)
                (source : 'a list) : CompilerMonad<'state, 'env, 'acc> = 
        CompilerMonad <| fun env accum -> 
            let rec work (st : 'state) 
                            (xs : 'a list) 
                            (ac : 'acc)
                            (fk : ErrMsg -> Result<'state * 'acc, ErrMsg>) 
                            (sk : 'state -> 'acc -> Result<'state * 'acc, ErrMsg>) = 
                match xs with
                | [] -> sk st ac
                | x1 :: rest -> 
                    match apply1 (action st x1) env ac with
                    | Error msg -> fk msg
                    | Ok (st1, ac1) -> 
                        work st1 rest ac1 fk sk
            work state source accum (fun msg -> Error msg) (fun st ac -> Ok (st,ac))



    let smapM (action : 'a -> CompilerMonad<'b, 'env, 'acc>) 
                (source : seq<'a>) : CompilerMonad<seq<'b>, 'env, 'acc> = 
        CompilerMonad <| fun env accum ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (ac : 'acc) 
                            (fk : ErrMsg -> Result<seq<'b> * 'acc, ErrMsg>) 
                            (sk : seq<'b> -> 'acc -> Result<seq<'b> * 'acc, ErrMsg>) = 
                if not (sourceEnumerator.MoveNext()) then 
                    sk Seq.empty ac
                else
                    let a1 = sourceEnumerator.Current
                    match apply1 (action a1) env ac with
                    | Error msg -> fk msg
                    | Ok (b1, ac1) -> 
                        work ac1 fk (fun sx ac2 -> 
                        sk (seq { yield b1; yield! sx }) ac2)
            work accum (fun msg -> Error msg) (fun ans ac -> Ok (ans, ac))

    let sforM (sx : seq<'a>) 
              (fn : 'a -> CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<seq<'b>, 'env, 'acc> = 
        smapM fn sx
    
    let smapMz (action : 'a -> CompilerMonad<'b, 'env, 'acc>) 
                (source : seq<'a>) : CompilerMonad<unit, 'env, 'acc> = 
        CompilerMonad <| fun env accum ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (ac : 'acc) 
                            (fk : ErrMsg -> Result<unit * 'acc, ErrMsg>) 
                            (sk : 'acc -> Result<unit * 'acc, ErrMsg>) = 
                if not (sourceEnumerator.MoveNext()) then 
                    sk ac
                else
                    let a1 = sourceEnumerator.Current
                    match apply1 (action a1) env ac with
                    | Error msg -> fk msg
                    | Ok (_, ac1) -> 
                        work ac1 fk sk
            work accum (fun msg -> Error msg) (fun ac -> Ok ((), ac))

    
    let sforMz (source : seq<'a>) 
                (action : 'a -> CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<unit, 'env, 'acc> = 
        smapMz action source

        
    let sfoldM (action : 'state -> 'a -> CompilerMonad<'state, 'env, 'acc>) 
                (state : 'state)
                (source : seq<'a>) : CompilerMonad<'state, 'env, 'acc> = 
        CompilerMonad <| fun env accum ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (st : 'state) 
                            (ac : 'acc)
                            (fk : ErrMsg -> Result<'state * 'acc, ErrMsg>) 
                            (sk : 'state -> 'acc -> Result<'state * 'acc, ErrMsg>) = 
                if not (sourceEnumerator.MoveNext()) then 
                    sk st ac
                else
                    let x1 = sourceEnumerator.Current
                    match apply1 (action st x1) env ac with
                    | Error msg -> fk msg
                    | Ok (st1, ac1) -> 
                        work st1 ac1 fk sk
            work state accum (fun msg -> Error msg) (fun ans ac -> Ok (ans, ac))


    /// Implemented in CPS 
    let mapiM (mf : int -> 'a -> CompilerMonad<'b, 'env, 'acc>) 
                (source : 'a list) : CompilerMonad<'b list, 'env, 'acc> = 
        CompilerMonad <| fun env accum -> 
            let rec work (xs : 'a list)
                         (count : int)
                         (ac : 'acc)
                         (fk : ErrMsg -> Result<'b list * 'acc, ErrMsg>) 
                         (sk : 'b list -> 'acc -> Result<'b list * 'acc, ErrMsg>) = 
                match xs with
                | [] -> sk [] ac
                | y :: ys -> 
                    match apply1 (mf count y) env ac with
                    | Error msg -> fk msg
                    | Ok (v1, ac1) -> 
                        work ys (count+1) ac1 fk (fun vs ac2 ->
                        sk (v1::vs) ac2)
            work source 0 accum (fun msg -> Error msg) (fun ans ac -> Ok (ans, ac))


    /// Implemented in CPS 
    let mapiMz (mf : int -> 'a -> CompilerMonad<'b, 'env, 'acc>) 
                (source : 'a list) : CompilerMonad<unit, 'env, 'acc> = 
        CompilerMonad <| fun env accum -> 
            let rec work (xs : 'a list) 
                         (count : int)
                         (ac : 'acc)
                         (fk : ErrMsg -> Result<unit * 'acc, ErrMsg>) 
                         (sk : 'acc -> Result<unit * 'acc, ErrMsg>) = 
                match xs with
                | [] -> sk ac
                | y :: ys -> 
                    match apply1 (mf count y) env ac with
                    | Error msg -> fk msg
                    | Ok (_, ac1) -> 
                        work ys (count+1) ac1 fk sk
            work source 0 accum (fun msg -> Error msg) (fun ac -> Ok ((), ac))

    

    let foriM (xs : 'a list) 
              (fn : int -> 'a -> CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<'b list, 'env, 'acc> = 
        mapiM fn xs

    let foriMz (xs : 'a list) 
               (fn : int -> 'a -> CompilerMonad<'b, 'env, 'acc>) : CompilerMonad<unit, 'env, 'acc> = 
        mapiMz fn xs


    /// Implemented in CPS 
    let filterM (mf: 'a -> CompilerMonad<bool, 'env, 'acc>) 
                (source : 'a list) : CompilerMonad<'a list, 'env, 'acc> = 
        CompilerMonad <| fun env accum -> 
            let rec work (xs : 'a list)
                         (ac : 'acc)
                         (fk : ErrMsg -> Result<'a list * 'acc, ErrMsg>) 
                         (sk : 'a list -> 'acc -> Result<'a list * 'acc, ErrMsg>) = 
                match xs with
                | [] -> sk [] ac
                | y :: ys -> 
                    match apply1 (mf y) env ac with
                    | Error msg -> fk msg
                    | Ok (test, ac1)  -> 
                        work ys ac1 fk (fun vs ac2 ->
                        let vs1 = if test then (y::vs)  else vs
                        sk vs1 ac2)
            work source accum (fun msg -> Error msg) (fun ans ac -> Ok (ans, ac))

