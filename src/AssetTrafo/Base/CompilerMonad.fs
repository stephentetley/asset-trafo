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


    let inline private bindM (action : CompilerMonad<'a, 'res, 'env, 'state>) 
                             (fn :'a -> CompilerMonad<'b, 'res, 'env, 'state>) 
                                : CompilerMonad<'b, 'res, 'env, 'state> =
        CompilerMonad <| fun handle env st -> 
            match apply1 action handle env st with
            | Error msg -> Error msg
            | Ok (a, st1) -> apply1 (fn a) handle env st1


    let inline private zeroM () : CompilerMonad<'a, 'res, 'env, 'state> = 
        CompilerMonad <| fun _ _ _ -> Error "zeroM"

    /// "First success" 
    /// If mfirst fails tray to apply msecond
    let inline private combineM (mfirst : CompilerMonad<'a, 'res, 'env, 'state>) 
                                (msecond : CompilerMonad<'a, 'res, 'env, 'state>) 
                                    : CompilerMonad<'a, 'res, 'env, 'state> = 
        CompilerMonad <| fun res env st -> 
            match apply1 mfirst res env st with
            | Error msg -> apply1 msecond res env st
            | Ok res -> Ok res

    let inline private delayM (fn:unit -> CompilerMonad<'a, 'res, 'env, 'state>) 
                                : CompilerMonad<'a, 'res, 'env, 'state> = 
        bindM (mreturn ()) fn 

    type CompilerMonadBuilder<'res, 'env, 'state>() = 
        member self.Return x : CompilerMonad<'a, 'res, 'env, 'state> = mreturn x
        member self.Bind (p,f) : CompilerMonad<'b, 'res, 'env, 'state> = bindM p f
        member self.Zero () : CompilerMonad<unit, 'res, 'env, 'state> = zeroM ()
        member self.Combine (ma,mb) : CompilerMonad<'a, 'res, 'env, 'state> = combineM ma mb
        member self.Delay fn : CompilerMonad<'a, 'res, 'env, 'state> = delayM fn
        member self.ReturnFrom(ma) : CompilerMonad<'a, 'res, 'env, 'state> = ma


    /// let (rewrite:ComplierMonadBuilder) = new ComplierMonadBuilder()

    let runCompilerMonad (action : CompilerMonad<'a, 'res, 'env, 'state>) 
                         (handle : 'res)
                         (env : 'env)
                         (state : 'state) : Result<'a * 'state, ErrMsg> =   
        apply1  action handle env state

    // ****************************************************
    // Errors

    let cmError (msg: string) : CompilerMonad<'a, 'res, 'env, 'state> = 
        CompilerMonad <| fun _ _ _ -> Error msg


    let swapError (msg : string) 
                  (action : CompilerMonad<'a, 'res, 'env, 'state>) 
                    : CompilerMonad<'a, 'res, 'env, 'state> = 
        CompilerMonad <| fun res env st -> 
            match apply1 action res env st with
            | Error _ -> Error msg
            | Ok a -> Ok a
    
    let ( <?> ) (action : CompilerMonad<'a, 'res, 'env, 'state>)
                (msg : string) : CompilerMonad<'a, 'res, 'env, 'state> = 
        swapError msg action

    /// Regular combinators

    let fmapM (action : CompilerMonad<'a, 'res, 'env, 'state>) 
              (fn : 'a -> 'b) : CompilerMonad<'b, 'res, 'env, 'state> = 
        CompilerMonad <| fun res env st -> 
            match apply1 action res env st with
            | Error msg -> Error msg
            | Ok (a, st1) -> Ok (fn a, st1)

    let ( |>> ) (action : CompilerMonad<'a, 'res, 'env, 'state>) 
                (fn : 'a -> 'b) 
                    : CompilerMonad<'b, 'res, 'env, 'state> = 
        fmapM action fn

    let ( <<| ) (fn : 'a -> 'b) 
                (action : CompilerMonad<'a, 'res, 'env, 'state>) 
                    : CompilerMonad<'b, 'res, 'env, 'state> =  
        fmapM action fn



    let ( >>= ) (action : CompilerMonad<'a, 'res, 'env, 'state>) 
                (fn :'a -> CompilerMonad<'b, 'res, 'env, 'state>) 
                    : CompilerMonad<'b, 'res, 'env, 'state> =
        bindM action fn 

    let ( =<< ) (fn :'a -> CompilerMonad<'b, 'res, 'env, 'state>) 
                (action : CompilerMonad<'a, 'res, 'env, 'state>) 
                    : CompilerMonad<'b, 'res, 'env, 'state> =
        bindM action fn 

    let ( <|> ) (mfirst : CompilerMonad<'a, 'res, 'env, 'state>) 
                (msecond : CompilerMonad<'a, 'res, 'env, 'state>) 
                    : CompilerMonad<'a, 'res, 'env, 'state> = 
        combineM mfirst msecond




    let optionalM (action : CompilerMonad<'a, 'res, 'env, 'state>) 
                    : CompilerMonad<'a option, 'res, 'env, 'state> = 
        CompilerMonad <| fun res env st -> 
            match apply1 action res env st with
            | Error msg -> Ok (None, st)
            | Ok (a, st1) -> Ok (Some a, st1)

    let assertM (test : CompilerMonad<bool, 'res, 'env, 'state>) 
                    : CompilerMonad<unit, 'res, 'env, 'state> =
        CompilerMonad <| fun res env st -> 
            match apply1 test res env st with
            | Ok (true, st1) -> Ok ((), st1)
            | _ -> Error "assertM"

    let choice (actions : CompilerMonad<'a, 'res, 'env, 'state> list) 
                    : CompilerMonad<'a, 'res, 'env, 'state> =
        let rec work xs =
            match xs with
            | [] -> cmError "choice"
            | f1 :: rest -> f1 <|> work rest
        work actions


