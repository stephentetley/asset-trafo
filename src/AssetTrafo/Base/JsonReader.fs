// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Base


module JsonReader =
    
    
    open FSharp.Data
    open AssetTrafo.Base.CompilerMonad
    
    type GenReader<'ans, 'src> = CompilerMonad<'ans, unit, 'src, unit>

    type JsonReader<'ans> = GenReader<'ans, JsonValue>

        
    let (jsonValue : CompilerMonadBuilder<unit, JsonValue, unit>) = 
        new CompilerMonadBuilder<unit, JsonValue, unit> ()


    type RecordReader<'ans> = GenReader<'ans, (string * JsonValue) []>

    let (jsonRecord : CompilerMonadBuilder<unit, (string* JsonValue) [], unit>) =
        new CompilerMonadBuilder<unit, (string * JsonValue) [], unit> ()



    let internal runGenReader (action : GenReader<'a, 'src>) (source : 'src) : Result<'a, ErrMsg> = 
        match runCompilerMonad action () source () with
        | Ok (ans, _) -> Ok ans
        | Error msg -> Error msg


    let runJsonReader (action : JsonReader<'a>) (source : JsonValue) : Result<'a, ErrMsg> = 
        runGenReader action source

    let readError (msg : string) : GenReader<'a, 'src>= 
        cmError msg

    let withJsonValue (fn : JsonValue -> Result<'a, ErrMsg>) : JsonReader<'a> = 
        CompilerMonad <| fun res env st -> 
            match fn env with
            | Error msg -> Error msg
            | Ok a -> Ok (a, st)

    let withRecordFields (fn : (string* JsonValue) [] -> Result<'a, ErrMsg>) : RecordReader<'a> = 
        CompilerMonad <| fun res env st -> 
            match fn env with
            | Error msg -> Error msg
            | Ok a -> Ok (a, st)

    let apply1 (action : GenReader<'a, 'src>) (source : 'src) : Result<'a, ErrMsg> = 
        runGenReader action source
                                

    let readNull : JsonReader<unit> = 
        withJsonValue <| fun src -> 
            match src with 
            | JsonValue.Null -> Ok () 
            | _ -> Error "not a Null"


    let readBoolean : JsonReader<bool> = 
        withJsonValue <| fun src -> 
            match src with 
            | JsonValue.Boolean ans -> Ok ans
            | _ -> Error "not a Boolean"

    let readFloat : JsonReader<float> = 
        withJsonValue <| fun src -> 
            match src with 
            | JsonValue.Float ans -> Ok ans
            | _ -> Error "not a Number"


    let readNumber : JsonReader<decimal> = 
        withJsonValue <| fun src -> 
            match src with 
            | JsonValue.Number ans -> Ok ans
            | _ -> Error "not a String"



    let readString : JsonReader<string> = 
        withJsonValue <| fun src -> 
            match src with 
            | JsonValue.String ans -> Ok ans
            | _ -> Error "not a String"

    let readArray (read1 : JsonReader<'a>) : JsonReader<'a []> = 
        withJsonValue <| fun src -> 
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



    let readRecord (readFields : RecordReader<'ans>) : JsonReader<'ans> = 
        withJsonValue <| fun src -> 
            match src with 
            | JsonValue.Record elements -> apply1 readFields elements
            | _ -> Error "not an Record"

    let readField (fieldName : string) (read1 : JsonReader<'a>) : RecordReader<'a> = 
        withRecordFields <| fun src -> 
            match Array.tryFind (fun (key,_) -> key = fieldName) src with 
            | Some (_, ans) -> apply1 read1 ans
            | None -> Error <| "field not found: " + fieldName


    let readInt : JsonReader<int> = 
        withJsonValue <| fun src -> 
            match src with 
            | JsonValue.Number d -> Ok (int d)
            | _ -> Error "not a Number"

    let readDictionary (read1 : JsonReader<'a>) : JsonReader<Map<string, 'a>> = 
        withJsonValue <| fun src -> 
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



