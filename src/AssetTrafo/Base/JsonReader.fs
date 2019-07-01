// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Base


module JsonReader =
    

    open FSharp.Data

    type ErrMsg = string 

    type GenReader<'ans, 'src> = GenReader of ('src -> Result<'ans, ErrMsg>)

    let private apply1 (ma : GenReader<'ans, 'src>)
                       (src : 'src) : Result<'ans, ErrMsg> = 
        let (GenReader f) = ma in f  src

    let private failM () : GenReader<'ans, 'src> = 
        GenReader <| fun _ -> Error "failM"

    let mreturn (x : 'ans) : GenReader<'ans, 'src> =
        GenReader <| fun _ -> Ok x

    let bindM (ma : GenReader<'a, 'src>)
              (next : 'a -> GenReader<'b, 'src>) : GenReader<'b, 'src> =
        GenReader <| fun src -> 
            match apply1 ma src with
            | Error msg -> Error msg
            | Ok ans -> apply1 (next ans) src

    let mzero () : GenReader<'ans, 'src> = 
        GenReader <| fun _ -> Error "mzero"

    /// mplus is first success
    let mplus (ma : GenReader<'ans, 'src>) 
              (mb : GenReader<'ans, 'src>) : GenReader<'ans, 'src> = 
        GenReader <| fun src ->
            match apply1 ma src with
            | Error _ -> apply1 mb src
            | Ok a -> Ok a
    
    let inline private delayM (fn:unit -> GenReader<'ans, 'src>) : GenReader<'ans, 'src> = 
        bindM (mreturn ()) fn 


    type GenReaderBuilder<'handle>() = 
        member self.Return x    : GenReader<'a, 'handle> = mreturn x
        member self.Bind (p,f)  : GenReader<'b, 'handle> = bindM p f
        member self.Zero ()     : GenReader<unit, 'handle> = mzero ()
        member self.Combine (ma, mb)    : GenReader<'ans, 'handle> = mplus ma mb
        member self.ReturnFrom(ma:GenReader<'ans, 'handle>) : GenReader<'ans, 'handle> = ma
        
    let (jsonValue:GenReaderBuilder<JsonValue>) = new GenReaderBuilder<JsonValue> ()

    type JsonReader<'a> = GenReader<'a, JsonValue>

    let runJsonReader (ma : JsonReader<'a>) (source : JsonValue) : Result<'a, ErrMsg> = 
        apply1 ma source


    let readError (msg : string) : GenReader<'ans, 'src> = 
        GenReader <| fun _ -> Error msg

    let ( <|> ) (ma : GenReader<'ans, 'src>) 
                (mb : GenReader<'ans, 'src>) : GenReader<'ans, 'src> = mplus ma mb


    let fmapM (ma : GenReader<'a, 'src>) (fn : 'a -> 'b) : GenReader<'b, 'src> = 
        GenReader <| fun src ->
            match apply1 ma src with
            | Error msg -> Error msg
            | Ok a -> Ok (fn a)

    let optionalM (ma : GenReader<'a, 'src>) : GenReader<'a option, 'src> = 
        GenReader <| fun src ->
            match apply1 ma src with
            | Error msg -> Ok None
            | Ok a -> Ok (Some a)


    let ( |>> ) (ma : GenReader<'a, 'src>) (fn : 'a -> 'b) : GenReader<'b, 'src> = fmapM ma fn

    let ( <<| ) (fn : 'a -> 'b) (ma : GenReader<'a, 'src>) : GenReader<'b, 'src> = fmapM ma fn

    let choice (readers: (GenReader<'ans, 'src>) list) : GenReader<'ans, 'src> =
        let rec work xs =
            match xs with
            | [] -> readError "choice"
            | f1 :: rest -> f1 <|> work rest
        work readers

    let assertM (test : GenReader<bool, 'src>) : GenReader<unit, 'src> = 
        GenReader <| fun src -> 
            match apply1 test src with
            | Ok true -> Ok ()
            | _ -> Error "assertM"


    let readNull : JsonReader<unit> = 
        GenReader <| fun src -> 
            match src with 
            | JsonValue.Null -> Ok () 
            | _ -> Error "not a Null"


    let readBoolean : JsonReader<bool> = 
        GenReader <| fun src -> 
            match src with 
            | JsonValue.Boolean ans -> Ok ans
            | _ -> Error "not a Boolean"

    let readFloat : JsonReader<float> = 
        GenReader <| fun src -> 
            match src with 
            | JsonValue.Float ans -> Ok ans
            | _ -> Error "not a Number"


    let readNumber : JsonReader<decimal> = 
        GenReader <| fun src -> 
            match src with 
            | JsonValue.Number ans -> Ok ans
            | _ -> Error "not a String"



    let readString : JsonReader<string> = 
        GenReader <| fun src -> 
            match src with 
            | JsonValue.String ans -> Ok ans
            | _ -> Error "not a String"

    let readArray (read1 : JsonReader<'a>) : JsonReader<'a []> = 
        GenReader <| fun src -> 
            let rec work elements fk sk = 
                match elements with
                | [] -> sk []
                | a1 :: rest -> 
                    match apply1 read1 a1 with
                    | Error msg -> fk msg
                    | Ok ans1 -> 
                        work rest fk (fun acc -> sk (ans1 :: acc))
            match src with 
            | JsonValue.Array elements -> 
                work (Array.toList elements) (fun msg -> Error msg) (fun xs -> Ok (Array.ofList xs)) 
            | _ -> Error "not an Array"

    type RecordReader<'ans> = GenReader<'ans, (string * JsonValue) []>
    
    let (jsonRecord:GenReaderBuilder<(string* JsonValue) []>) = new GenReaderBuilder<(string * JsonValue) []> ()


    let readRecord (readFields : RecordReader<'ans>) : JsonReader<'ans> = 
        GenReader <| fun src -> 
            match src with 
            | JsonValue.Record elements -> apply1 readFields elements
            | _ -> Error "not an Record"

    let readField (fieldName : string) (read1 : JsonReader<'a>) : RecordReader<'a> = 
        GenReader <| fun src -> 
            match Array.tryFind (fun (key,_) -> key = fieldName) src with 
            | Some (_, ans) -> apply1 read1 ans
            | None -> Error <| "field not found: " + fieldName


    let readInt : JsonReader<int> = 
        GenReader <| fun src -> 
            match src with 
            | JsonValue.Number d -> Ok (int d)
            | _ -> Error "not a Number"

    let readDictionary (read1 : JsonReader<'a>) : JsonReader<Map<string, 'a>> = 
        GenReader <| fun src -> 
            let rec work elements fk sk = 
                match elements with
                | [] -> sk Map.empty
                | (key,a1) :: rest -> 
                    match apply1 read1 a1 with
                    | Error msg -> fk msg
                    | Ok ans1 -> 
                        work rest fk (fun acc -> sk (Map.add  key ans1 acc))
            match src with 
            | JsonValue.Record elements -> 
                work (Array.toList elements) (fun msg -> Error msg) (fun acc -> Ok acc) 
            | _ -> Error "not an Array"



