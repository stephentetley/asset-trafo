// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Base


module JsonWriter =
    

    open FSharp.Data

    type ErrMsg = string

    type Ans<'ans> = Result<'ans, ErrMsg>
    
    type Fk<'ans> = ErrMsg -> Ans<'ans>
    
    type Sk<'a, 'ans> = 'a -> Fk<'ans> -> Ans<'ans>  

    type JsonWriter<'a, 'ans> = JsonWriter of (Sk<'a, 'ans> -> Fk<'ans> -> Ans<'ans>)

    let private apply1 (ma : JsonWriter<'a, 'ans>)
                       (success : Sk<'a, 'ans>)
                       (failure : Fk<'ans>) : Ans<'ans> = 
        let (JsonWriter f) = ma in f success failure

    let private failM () : JsonWriter<'a, 'ans> = 
        JsonWriter <| fun _ fk -> fk "failM"

    let mreturn (x : 'a) : JsonWriter<'a, 'ans> =
        JsonWriter <| fun sk fk -> sk x fk

    let bindM (ma : JsonWriter<'a, 'ans>)
              (next : 'a -> JsonWriter<'b, 'ans>) : JsonWriter<'b, 'ans> =
        JsonWriter <| fun sk -> 
            apply1 ma (fun a -> apply1 (next a) sk)

    let mzero () : JsonWriter<'a, 'ans> = 
        JsonWriter <| fun _ _ -> Error "mzero"

    let mplus (ma : JsonWriter<'a, 'ans>) 
              (mb : JsonWriter<'a, 'ans>) : JsonWriter<'a, 'ans> = 
        JsonWriter <| fun sk fk ->
            match apply1 ma sk fk with
            | Error _ -> apply1 mb sk fk
            | Ok a -> Ok a
    
    let inline private delayM (fn:unit -> JsonWriter<'a, 'ans>) : JsonWriter<'a, 'ans> = 
        bindM (mreturn ()) fn 


    type JsonWriterBuilder() = 
        member self.Return x    = mreturn x
        member self.Bind (p,f)  = bindM p f
        member self.Zero ()     = mzero ()
        member self.Combine (ma, mb) = mplus ma mb
        member self.ReturnFrom(ma:JsonWriter<'a, 'ans>) : JsonWriter<'a, 'ans> = ma
        
    let (worksf:JsonWriterBuilder) = new JsonWriterBuilder()

    let runJsonWriter (ma : JsonWriter<'a, 'a>) : Ans<'a> = 
        apply1 ma (fun a _ -> Ok a) (fun msg -> Error msg)

    type JsonWrite<'a> = JsonWriter<'a, JsonValue>

    


    let resultError (msg : string) : JsonWriter<'a, 'ans> = 
        JsonWriter <| fun _ _ -> Error msg

    let jsInt (i : int) : JsonWrite<_> = mreturn (JsonValue.Number (decimal i))

    let jsString (s : string) : JsonWrite<_> = mreturn (JsonValue.String s)