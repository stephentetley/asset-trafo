// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module CompilerMonad =
    
    open System
    open FSharp.Core

    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.EquiIndexing
    open AssetPatch.TemplatePatcher.Template

 

    // May get extended...
    type CompilerEnv = 
        { UserName : string 
          TemplateEnv : TemplateEnv
          FlocVariant : string option
          EquiVariant : string option
         }

            
    let defaultCompilerEnv (userName : string) (templateEnv : TemplateEnv) : CompilerEnv = 
        { UserName = userName
          TemplateEnv = templateEnv
          FlocVariant = None
          EquiVariant = None }
    

    /// CompilerMonad is a Reader-Error-State(name supply) monad.
    type CompilerMonad<'a> = 
        private | CompilerMonad of (CompilerEnv -> Result<'a, ErrMsg>)

    let inline private apply1 (ma : CompilerMonad<'a>) 
                                (env : CompilerEnv) : Result<'a, ErrMsg> = 
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

    type CompilerOptions = 
        { UseInterimFlocIds : bool 
          UserName : string }

    let runCompiler (options : CompilerOptions) 
                    (equiIndexFile : string option)
                    (action : CompilerMonad<'a> ) : Result<'a, ErrMsg> =         
        let indices = 
            match equiIndexFile with 
            | None -> Ok emptyEquiMap
            | Some path -> readEquiDownload path
        let env1  = 
            Result.map (fun ans -> 
                        { CurrentFloc = FuncLocPath.Create("*****")
                          Properties = defaultEnvProperties ()
                          EquipmentIndices = ans }) indices     
        
        match env1 with
        | Error msg -> Error msg
        | Ok env -> apply1 action (defaultCompilerEnv options.UserName env) 

    


    // ************************************************************************
    // Usual monadic operations


    // Common operations
    let fmapM (update : 'a -> 'b) 
              (action : CompilerMonad<'a>) : CompilerMonad<'b> = 
        compile {
            let! a = action
            return (update a)
            }
            
       
    /// Operator for fmap.
    let ( |>> ) (action : CompilerMonad<'a>) 
                (update : 'a -> 'b) : CompilerMonad<'b> = 
        fmapM update action

    /// Flipped fmap.
    let ( <<| ) (update : 'a -> 'b) 
                (action : CompilerMonad<'a>) : CompilerMonad<'b> = 
        fmapM update action

    /// Haskell Applicative's (<*>)
    let apM (mf : CompilerMonad<'a ->'b>) 
            (ma : CompilerMonad<'a>) : CompilerMonad<'b> = 
        compile { 
            let! fn = mf
            let! a = ma
            return (fn a) 
        }

    /// Operator for apM
    let ( <*> ) (ma : CompilerMonad<'a -> 'b>) 
                (mb : CompilerMonad<'a>) : CompilerMonad<'b> = 
        apM ma mb

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

    /// Perform two actions in sequence. 
    /// Ignore the results of the second action if both succeed.
    let seqL (action1 : CompilerMonad<'a>) 
             (action2 : CompilerMonad<'b>) : CompilerMonad<'a> = 
        compile { 
            let! a = action1
            let! _ = action2
            return a
        }

    /// Operator for seqL
    let (.>>) (action1 : CompilerMonad<'a>) 
                (action2 : CompilerMonad<'b>) : CompilerMonad<'a> = 
        seqL action1 action2

    /// Perform two actions in sequence. 
    /// Ignore the results of the first action if both succeed.
    let seqR (action1 : CompilerMonad<'a>) 
             (action2 : CompilerMonad<'b>) : CompilerMonad<'b> = 
        compile { 
            let! _ = action1
            let! b = action2
            return b
        }

    /// Operator for seqR
    let (>>.) (action1 : CompilerMonad<'a>) 
              (action2 : CompilerMonad<'b>) : CompilerMonad<'b> = 
        seqR action1 action2


    // ************************************************************************
    // Errors

    let throwError (msg : string) : CompilerMonad<'a> = 
        CompilerMonad <| fun _ -> Error msg


    let swapError (newMessage : string) 
                  (ma : CompilerMonad<'a>) : CompilerMonad<'a> = 
        CompilerMonad <| fun env -> 
            apply1 ma env |> Result.mapError (fun _ -> newMessage)
            

    /// Operator for flip swapError
    let ( <?> ) (action : CompilerMonad<'a>) (msg : string) : CompilerMonad<'a> = 
        swapError msg action
    
    let augmentError (update : string -> string) 
                     (action : CompilerMonad<'a>) : CompilerMonad<'a> = 
        CompilerMonad <| fun env ->
            apply1 action env |> Result.mapError update



    // ************************************************************************
    // Reader operations

    let ask () : CompilerMonad<CompilerEnv> = 
        CompilerMonad <| fun env -> Ok env

    let asks (projection : CompilerEnv -> 'ans) : CompilerMonad<'ans> = 
        CompilerMonad <| fun env -> Ok (projection env)

    let local (modify : CompilerEnv -> CompilerEnv) (ma : CompilerMonad<'ans>) : CompilerMonad<'ans> = 
        CompilerMonad <| fun env -> apply1 ma (modify env)


    // ************************************************************************
    // Alternatives and Optionals...

    let ( <|> ) (action1 : CompilerMonad<'a>)  
                (action2 : CompilerMonad<'a>) : CompilerMonad<'a> = 
        altM action1 action2

    let liftOption (opt : Option<'a>) : CompilerMonad<'a> = 
        match opt with
        | Some ans -> mreturn ans
        | None -> throwError "liftOption None"

    let liftResult (result : Result<'a, ErrMsg>) : CompilerMonad<'a> = 
        match result with
        | Ok ans -> mreturn ans
        | Error err -> throwError err

    let liftResultBy (errProjection : 'Err -> ErrMsg) 
                     (result : Result<'a, 'Err>) : CompilerMonad<'a> = 
        match result with
        | Ok ans -> mreturn ans
        | Error err -> throwError (errProjection err)

    /// Evaluate a thunk that may trow an exception (cf Haskell IO)
    let liftAction (action : unit -> 'a) : CompilerMonad<'a> = 
        try 
            action () |> mreturn
        with
        | ex -> throwError ex.Message

    /// Try to run a computation.
    /// On failure, recover or throw again with the handler.
    let attempt (action : CompilerMonad<'a>) 
                (handler : ErrMsg -> CompilerMonad<'a>) : CompilerMonad<'a> = 
        CompilerMonad <| fun env ->
            match apply1 action env with
            | Ok ans -> Ok ans
            | Error msg -> apply1 (handler msg) env


    let assertM (cond : CompilerMonad<bool>) 
                (errMsg : string) : CompilerMonad<unit> = 
        compile {
            match! cond with
            | true -> return ()
            | false -> return! throwError errMsg
        }

    let assertBool (cond : bool) 
                    (errMsg : string) : CompilerMonad<unit> = 
        compile {
            match cond with
            | true -> return ()
            | false -> return! throwError errMsg
        }

    /// Run a potentially failing action. If it succeeds the answer
    /// is wrapped in ``Some``. 
    /// If it fails trap the error and return ``None``.
    let optional (action : CompilerMonad<'a>) : CompilerMonad<'a option> = 
        attempt (action |>> Some) (fun _ -> mreturn None)

    /// Run an optional action - if it returns ``Some a`` return the 
    /// answer. If it returns ``None`` the fail.
    let getOptional (action : CompilerMonad<'a option>) : CompilerMonad<'a> = 
        compile { 
            match! action with
            | Some a -> return a
            | None -> return! throwError "getOptional - None" 
        }




    let choice (actions : CompilerMonad<'a> list) : CompilerMonad<'a> = 
        CompilerMonad <| fun env -> 
            /// State doesn't need to be in the worker
            let rec work acts cont = 
                match acts with 
                | [] -> Error "choice"
                | action1 :: rest ->
                    match apply1 action1 env with
                    | Ok ans -> cont (Ok ans)
                    | Error _ -> work rest cont
            work actions (fun x -> x)

    // ************************************************************************
    // liftM2 etc

    // liftM (which is fmap)
    let liftM (fn : 'a -> 'ans) 
                (action : CompilerMonad<'a>) : CompilerMonad<'ans> = 
        fmapM fn action

    let liftM2 (combine : 'a -> 'b -> 'ans) 
                (action1 : CompilerMonad<'a>) 
                (action2 : CompilerMonad<'b>) : CompilerMonad<'ans> = 
        compile { 
            let! a = action1
            let! b = action2
            return (combine a b)
        }

    let liftM3 (combine : 'a -> 'b -> 'c -> 'ans) 
                (action1 : CompilerMonad<'a>) 
                (action2 : CompilerMonad<'b>) 
                (action3 : CompilerMonad<'c>) : CompilerMonad<'ans> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            return (combine a b c)
        }

    let liftM4 (combine : 'a -> 'b -> 'c -> 'd -> 'ans) 
                (action1 : CompilerMonad<'a>) 
                (action2 : CompilerMonad<'b>) 
                (action3 : CompilerMonad<'c>) 
                (action4 : CompilerMonad<'d>) : CompilerMonad<'ans> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            return (combine a b c d)
        }


    let liftM5 (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'ans) 
                (action1 : CompilerMonad<'a>) 
                (action2 : CompilerMonad<'b>) 
                (action3 : CompilerMonad<'c>) 
                (action4 : CompilerMonad<'d>) 
                (action5 : CompilerMonad<'e>) : CompilerMonad<'ans> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            let! e = action5
            return (combine a b c d e)
        }

    let liftM6 (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'ans) 
                (action1 : CompilerMonad<'a>) 
                (action2 : CompilerMonad<'b>) 
                (action3 : CompilerMonad<'c>) 
                (action4 : CompilerMonad<'d>) 
                (action5 : CompilerMonad<'e>) 
                (action6 : CompilerMonad<'f>) : CompilerMonad<'ans> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            let! e = action5
            let! f = action6
            return (combine a b c d e f)
        }

    let tupleM2 (action1 : CompilerMonad<'a>) 
                (action2 : CompilerMonad<'b>) : CompilerMonad<'a * 'b> = 
        liftM2 (fun a b -> (a,b)) action1 action2

    let tupleM3 (action1 : CompilerMonad<'a>) 
                (action2 : CompilerMonad<'b>) 
                (action3 : CompilerMonad<'c>) : CompilerMonad<'a * 'b * 'c> = 
        liftM3 (fun a b c -> (a,b,c)) action1 action2 action3

    let tupleM4 (action1 : CompilerMonad<'a>) 
                (action2 : CompilerMonad<'b>) 
                (action3 : CompilerMonad<'c>) 
                (action4 : CompilerMonad<'d>) : CompilerMonad<'a * 'b * 'c * 'd> = 
        liftM4 (fun a b c d -> (a,b,c,d)) action1 action2 action3 action4

    let tupleM5 (action1 : CompilerMonad<'a>) 
                (action2 : CompilerMonad<'b>) 
                (action3 : CompilerMonad<'c>) 
                (action4 : CompilerMonad<'d>) 
                (action5 : CompilerMonad<'e>) : CompilerMonad<'a * 'b * 'c * 'd * 'e> = 
        liftM5 (fun a b c d e -> (a,b,c,d,e)) action1 action2 action3 action4 action5

    let tupleM6 (action1 : CompilerMonad<'a>) 
                (action2 : CompilerMonad<'b>) 
                (action3 : CompilerMonad<'c>) 
                (action4 : CompilerMonad<'d>) 
                (action5 : CompilerMonad<'e>) 
                (action6 : CompilerMonad<'f>) : CompilerMonad<'a * 'b * 'c * 'd * 'e * 'f> = 
        liftM6 (fun a b c d e f -> (a,b,c,d,e,f)) 
                action1 action2 action3 action4 action5 action6



    let pipeM2 (action1 : CompilerMonad<'a>) 
               (action2 : CompilerMonad<'b>) 
               (combine:'a -> 'b -> 'ans) : CompilerMonad<'ans> = 
        liftM2 combine action1 action2

    let pipeM3 (action1 : CompilerMonad<'a>) 
               (action2 : CompilerMonad<'b>) 
               (action3 : CompilerMonad<'c>) 
               (combine : 'a -> 'b -> 'c -> 'ans) : CompilerMonad<'ans> = 
        liftM3 combine action1 action2 action3

    let pipeM4 (action1 : CompilerMonad<'a>) 
               (action2 : CompilerMonad<'b>) 
               (action3 : CompilerMonad<'c>) 
               (action4 : CompilerMonad<'d>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'ans) : CompilerMonad<'ans> = 
        liftM4 combine action1 action2 action3 action4

    let pipeM5 (action1 : CompilerMonad<'a>) 
               (action2 : CompilerMonad<'b>) 
               (action3 : CompilerMonad<'c>) 
               (action4 : CompilerMonad<'d>) 
               (action5 : CompilerMonad<'e>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'ans) : CompilerMonad<'ans> = 
        liftM5 combine action1 action2 action3 action4 action5

    let pipeM6 (action1 : CompilerMonad<'a>) 
               (action2 : CompilerMonad<'b>) 
               (action3 : CompilerMonad<'c>) 
               (action4 : CompilerMonad<'d>) 
               (action5 : CompilerMonad<'e>) 
               (action6 : CompilerMonad<'f>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'ans) : CompilerMonad<'ans> = 
        liftM6 combine action1 action2 action3 action4 action5 action6

    // ************************************************************************
    // Templates

    let evalTemplate (rootFloc : FuncLocPath) (code : Template<'a>) : CompilerMonad<'a> = 
        CompilerMonad <| fun env ->
            let env1 = { env.TemplateEnv with CurrentFloc = rootFloc }
            match runTemplate env1 code with
            | Ok a -> Ok a
            | Error msg -> Error msg

    // ************************************************************************
    // Traversals

    /// Implemented in CPS 
    let mapM (mf: 'a -> CompilerMonad<'b>) 
             (source : 'a list) : CompilerMonad<'b list> = 
        CompilerMonad <| fun env -> 
            let rec work (xs : 'a list)
                         (fk : ErrMsg -> Result<'b list, ErrMsg>) 
                         (sk : 'b list -> Result<'b list, ErrMsg>) = 
                match xs with
                | [] -> sk []
                | x :: rest -> 
                    match apply1 (mf x) env with
                    | Error msg -> fk msg
                    | Ok v1 -> 
                        work rest fk (fun vs ->
                        sk (v1::vs))
            work source (fun msg -> Error msg) (fun ans -> Ok ans)

    let forM (xs : 'a list) 
             (fn : 'a -> CompilerMonad<'b>) : CompilerMonad<'b list> = 
        mapM fn xs


    /// Implemented in CPS 
    let mapMz (mf: 'a -> CompilerMonad<'b>) 
              (source : 'a list) : CompilerMonad<unit> = 
        CompilerMonad <| fun env -> 
            let rec work (xs : 'a list)
                         (fk : ErrMsg -> Result<unit, ErrMsg>) 
                         (sk : unit -> Result<unit, ErrMsg>) = 
                match xs with
                | [] -> sk ()
                | x :: rest ->
                    match apply1 (mf x) env with
                    | Error msg -> fk msg
                    | Ok _ -> 
                        work rest fk sk
            work source  (fun msg -> Error msg) (fun _ -> Ok ())


    let forMz (xs : 'a list) 
              (fn : 'a -> CompilerMonad<'b>) : CompilerMonad<unit> = 
        mapMz fn xs

    let foldM (action : 'acc -> 'a -> CompilerMonad<'acc>) 
                (initial : 'acc)
                (source : 'a list) : CompilerMonad<'acc> = 
        CompilerMonad <| fun env -> 
            let rec work (acc : 'acc) 
                            (xs : 'a list) 
                            (fk : ErrMsg -> Result<'acc, ErrMsg>) 
                            (sk : 'acc -> Result<'acc, ErrMsg>) = 
                match xs with
                | [] -> sk acc
                | x1 :: rest -> 
                    match apply1 (action acc x1) env with
                    | Error msg -> fk msg
                    | Ok acc1 -> 
                        work acc1 rest fk sk
            work initial source (fun msg -> Error msg) (fun a -> Ok a)



    let smapM (action : 'a -> CompilerMonad<'b>) 
                (source : seq<'a>) : CompilerMonad<seq<'b>> = 
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
            work (fun msg -> Error msg) (fun a -> Ok a)

    let sforM (sx : seq<'a>) 
              (fn : 'a -> CompilerMonad<'b>) : CompilerMonad<seq<'b>> = 
        smapM fn sx
    
    let smapMz (action : 'a -> CompilerMonad<'b>) 
                (source : seq<'a>) : CompilerMonad<unit> = 
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
            work (fun msg -> Error msg) (fun _ -> Ok ())

    
    let sforMz (source : seq<'a>) 
                (action : 'a -> CompilerMonad<'b>) : CompilerMonad<unit> = 
        smapMz action source

        
    let sfoldM (action : 'acc -> 'a -> CompilerMonad<'acc>) 
                (initial : 'acc)
                (source : seq<'a>) : CompilerMonad<'acc> = 
        CompilerMonad <| fun env ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (acc : 'acc) 
                            (fk : ErrMsg -> Result<'acc, ErrMsg>) 
                            (sk : 'acc -> Result<'acc, ErrMsg>) = 
                if not (sourceEnumerator.MoveNext()) then 
                    sk acc
                else
                    let x = sourceEnumerator.Current
                    match apply1 (action acc x) env with
                    | Error msg -> fk msg
                    | Ok acc1 -> work acc1 fk sk
            work initial (fun msg -> Error msg) (fun a -> Ok a)


    /// Implemented in CPS 
    let mapiM (mf : int -> 'a -> CompilerMonad<'b>) 
                (source : 'a list) : CompilerMonad<'b list> = 
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
                    | Ok v1 -> 
                        work ys (count+1) fk (fun vs ->
                        sk (v1::vs))
            work source 0 (fun msg -> Error msg) (fun a -> Ok a)


    /// Implemented in CPS 
    let mapiMz (mf : int -> 'a -> CompilerMonad<'b>) 
                (source : 'a list) : CompilerMonad<unit> = 
        CompilerMonad <| fun env  -> 
            let rec work (xs : 'a list) 
                         (count : int)
                         (fk : ErrMsg -> Result<unit, ErrMsg>) 
                         (sk : unit -> Result<unit, ErrMsg>) = 
                match xs with
                | [] -> sk ()
                | y :: ys -> 
                    match apply1 (mf count y) env with
                    | Error msg -> fk msg
                    | Ok _ -> work ys (count+1)  fk sk
            work source 0 (fun msg -> Error msg) (fun _ -> Ok ())

    

    let foriM (xs : 'a list) 
              (fn : int -> 'a -> CompilerMonad<'b>) : CompilerMonad<'b list> = 
        mapiM fn xs

    let foriMz (xs : 'a list) 
               (fn : int -> 'a -> CompilerMonad<'b>) : CompilerMonad<unit> = 
        mapiMz fn xs


    /// Implemented in CPS 
    let filterM (mf: 'a -> CompilerMonad<bool>) 
                (source : 'a list) : CompilerMonad<'a list> = 
        CompilerMonad <| fun env -> 
            let rec work (xs : 'a list)
                         (fk : ErrMsg -> Result<'a list, ErrMsg>) 
                         (sk : 'a list -> Result<'a list, ErrMsg>) = 
                match xs with
                | [] -> sk []
                | y :: ys -> 
                    match apply1 (mf y) env with
                    | Error msg -> fk msg
                    | Ok test -> 
                        work ys fk (fun vs ->
                        let vs1 = if test then (y::vs) else vs
                        sk vs1)
            work source (fun msg -> Error msg) (fun a -> Ok a)

