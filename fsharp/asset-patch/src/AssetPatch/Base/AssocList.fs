// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

[<AutoOpen>]
module AssocListType = 

    [<Struct>]
    type AssocList<'Key,'T> = 
        internal | AssocList of ('Key * 'T) list

        member internal x.Assocs
            with get () : ('Key * 'T) list = let (AssocList xs) = x in xs

        static member Cons (key: 'Key, value: 'T, tail: AssocList<'Key, 'T>) : AssocList<'Key, 'T> = 
            let (AssocList xs) = tail in AssocList((key,value) :: xs)

[<RequireQualifiedAccess>]
module AssocList =

    let empty : AssocList<'Key, 'T> = 
        AssocList []

    let cons (key: 'Key) (value: 'T) (assocs : AssocList<'Key, 'T>) : AssocList<'Key, 'T> = 
        AssocList.Cons(key, value, assocs)

    let snoc (assocs : AssocList<'Key, 'T>) (key: 'Key) (value: 'T)  : AssocList<'Key, 'T> = 
        let xs = assocs.Assocs
        AssocList(xs @ [(key, value)])

    
    let ofList (elements: ('Key * 'T) list) : AssocList<'Key, 'T> = 
        AssocList(elements)

    let toList (assocs : AssocList<'Key,'T>) : ('Key * 'T) list = 
        assocs.Assocs

    let toListBy (mapper : 'Key -> 'T -> 'U) (assocs : AssocList<'Key,'T>) : 'U list = 
        let rec work xs cont = 
            match xs with 
            | [] -> cont []
            | (k,v) :: rest -> 
                work rest (fun vs -> 
                let v1 = mapper k v 
                cont (v1 :: vs))
        work assocs.Assocs (fun xs -> xs)

    let tryFind (key : 'Key) (source : AssocList<'Key, 'T>) : 'T option = 
        List.tryFind (fun x -> fst x = key) source.Assocs
            |> Option.map snd
            
    let tryFindKey (predicate : 'Key -> 'T -> bool) (source : AssocList<'Key, 'T>) : 'T option = 
        List.tryFind (fun (k,v) -> predicate k v) source.Assocs
            |> Option.map snd

    let tryFind2 (key1 : 'Key) (key2 : 'Key) (source : AssocList<'Key, 'T>) : ('T * 'T) option = 
        Option.map2 (fun x y -> (x,y))
                    (tryFind key1 source) 
                    (tryFind key2 source) 

    let tryFind3 (key1 : 'Key) (key2 : 'Key) (key3 : 'Key) 
                 (source : AssocList<'Key, 'T>) : ('T * 'T * 'T) option = 
        Option.map3 (fun x y z -> (x,y,z))
                    (tryFind key1 source) 
                    (tryFind key2 source) 
                    (tryFind key3 source)

    let tryFind4 (key1 : 'Key) (key2 : 'Key) (key3 : 'Key) (key4: 'Key) 
                 (source : AssocList<'Key, 'T>) : ('T * 'T * 'T * 'T) option = 
        match tryFind key1 source, tryFind key2 source, 
                tryFind key3 source, tryFind key4 source with
        | Some v1, Some v2, Some v3, Some v4 -> Some (v1, v2, v3, v4) 
        | _, _, _, _ -> None


    let tryFind5 (key1 : 'Key) (key2 : 'Key) (key3 : 'Key) 
                    (key4: 'Key) (key5: 'Key)
                    (source : AssocList<'Key, 'T>) : ('T * 'T * 'T * 'T * 'T) option = 
        match tryFind key1 source, tryFind key2 source, 
                tryFind key3 source, tryFind key4 source,
                tryFind key5 source with
        | Some v1, Some v2, Some v3, Some v4, Some v5-> Some (v1, v2, v3, v4, v5) 
        | _, _, _, _, _ -> None

    let update (key: 'Key) (value: 'T) (source: AssocList<'Key, 'T>) : AssocList<'Key, 'T> when 'Key : equality  = 
        let rec work xs cont = 
            match xs with 
            | [] -> cont []
            | (k1,_) as x :: rest -> 
                if k1 = key then 
                    cont ((key,value) :: rest)
                else
                    work rest (fun vs -> 
                    cont (x :: vs))
        work source.Assocs (fun xs -> AssocList(xs))

    let upsert (key: 'Key) (value: 'T) (source: AssocList<'Key, 'T>) : AssocList<'Key, 'T> when 'Key : equality  = 
        let rec work xs cont = 
            match xs with 
            | [] -> cont [(key, value)]
            | (k1,_) as x :: rest -> 
                if k1 = key then 
                    cont ((key,value) :: rest)
                else
                    work rest (fun vs -> 
                    cont (x :: vs))
        work source.Assocs (fun xs -> AssocList(xs))
    
    let remove (key : 'Key) 
                (source: AssocList<'Key, 'T>) : AssocList<'Key, 'T> when 'Key : equality  = 
        source.Assocs 
            |> List.filter (fun x -> fst x <> key)
            |> AssocList

    let removes (keys : seq<'Key>) 
                (source: AssocList<'Key, 'T>) : AssocList<'Key, 'T> when 'Key : equality  = 
        Seq.fold (fun st k -> remove k st) source keys

    let keys (source : AssocList<'Key, 'T>) : 'Key [] =
        source |> toListBy (fun k _ -> k) |> List.toArray

    let prioritize1 (key : 'Key) 
                    (assocs : AssocList<'Key, 'T>) : AssocList<'Key, 'T> when 'Key : equality  = 
        match tryFind key assocs with
        | None -> assocs
        | Some value -> 
            remove key assocs 
                |> fun xs -> AssocList.Cons(key, value, xs)
                

    let prioritize (keys : 'Key list) 
                   (assocs : AssocList<'Key, 'T>) : AssocList<'Key, 'T> when 'Key : equality  = 
        List.foldBack prioritize1 keys assocs


    /// Create a new assoc list by selecting fields of interest from the original 
    /// list.
    let select (keys : 'Key list) 
               (assocs : AssocList<'Key, 'T>) : AssocList<'Key, 'T> when 'Key : equality  = 
        let add1 key acc = 
            match tryFind key assocs with
            | None -> acc
            | Some value -> AssocList.Cons(key, value, acc)                   
        List.foldBack add1 keys empty