// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Base


module Attributes =
    
    open FSharp.Data

    open AssetTrafo.Base.JsonReader

    type AttrValue = 
        | AttrBool of bool
        | AttrString of string
        member x.ToJsonValue () : JsonValue = 
            match x with
            | AttrBool bool -> JsonValue.Boolean bool
            | AttrString str -> JsonValue.String str
            
        static member ReadJson () : JsonReader<AttrValue> = 
            choice [ readBoolean    |>> AttrBool
                   ; readString     |>> AttrString
                   ]
            

    [<Struct>]
    type Attributes = 
        | Attributes of Map<string, AttrValue>
        
        member x.Attrs 
            with get () : Map<string, AttrValue> = 
                match x with | Attributes(x) -> x

        member x.ToJsonValue () : JsonValue = 
            x.Attrs 
                |> Map.toArray
                |> Array.map (fun (n,v) -> (n, v.ToJsonValue() )) 
                |> JsonValue.Record 

        static member ReadJson () :  JsonReader<Attributes> = 
            readDictionary (AttrValue.ReadJson ()) |>> Attributes


            
