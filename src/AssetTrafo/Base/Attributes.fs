// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Base


module Attributes =
    
    open FSharp.Data
    
    open AssetTrafo.Base.CompilerMonad
    open AssetTrafo.Base.JsonReader

    type AttrValue = 
        | AttrBool of bool
        | AttrInt of int
        | AttrString of string
        member x.ToJsonValue () : JsonValue = 
            match x with
            | AttrBool ans -> JsonValue.Boolean ans
            | AttrInt ans -> JsonValue.Number (decimal ans)
            | AttrString ans -> JsonValue.String ans
            
        static member ReadJson () : JsonReader<AttrValue> = 
            choice [ readBoolean    |>> AttrBool
                   ; readInt        |>> AttrInt
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

        member x.Add(name: string, value: AttrValue) = 
            x.Attrs 
                |> Map.add name value
                |> Attributes

        static member Empty : Attributes = 
            Attributes Map.empty

        static member ReadJson () :  JsonReader<Attributes> = 
            readDictionary (AttrValue.ReadJson ()) |>> Attributes


    let add (name: string) (value: AttrValue) (attrs : Attributes) : Attributes =
        attrs.Add(name, value)
 