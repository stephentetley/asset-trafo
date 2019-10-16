// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangeRequest


module Syntax =

    open System

    [<Struct>]
    [<CustomComparison; CustomEquality>]
    type FlocPath = 
        | FlocPath of string list

        override x.ToString() = 
            let (FlocPath xs) = x in xs |> List.rev |> String.concat "-"

        override x.Equals(yobj) = 
            match yobj with
            | :? FlocPath as y -> x.ToString() = y.ToString()
            | _ -> false

        override x.GetHashCode () = 
            let (FlocPath xs) = x in hash xs

        member x.Level 
            with get () : int = 
                let (FlocPath xs) = x in xs.Length

        // Fails on malformed FlocPaths
        member x.Tip 
            with get () : string = 
                let (FlocPath xs) = x in 
                match xs with 
                | tip :: _ -> tip
                | [] -> failwith "Tip on empty"

        member x.Extend (code: string) : FlocPath = 
            let (FlocPath xs) = x in FlocPath (code :: xs)

        interface System.IEquatable<FlocPath> with
            member x.Equals(y) = 
                x.ToString() = y.ToString()

        interface System.IComparable with
            member x.CompareTo(yobj) = 
                match yobj with
                | :? FlocPath as y -> 
                    match compare x.Level y.Level with 
                    | 0 -> x.ToString().CompareTo(y.ToString()) 
                    | x -> x
                | _ -> failwith "Invalid CompareTo on FlocPath"

        static member Create (path : string) : FlocPath  = 
            let pathArr = path.Split( [| '-' |])
            FlocPath (pathArr |> Array.toList |> List.rev)


    let compareFlocPaths (x : FlocPath) (y : FlocPath) : int =
            match compare x.Level y.Level with 
            | 0 -> x.ToString().CompareTo(y.ToString()) 
            | x -> x


    /// Lookup name by floc if None.
    type FlocRequest = 
        { Path : FlocPath 
          Name : string option }


    type EquipmentRequest =
        { Path : FlocPath 
          Code : uint64
          Name : string }

    
    type ChangeRequest = 
        | FlocRequest of FlocRequest
        | EquipmentRequest of EquipmentRequest


    //let compareFlocChange (x : ChangeRequest) (y : ChangeRequest) : int =
    //    compare x.Path y.Path

