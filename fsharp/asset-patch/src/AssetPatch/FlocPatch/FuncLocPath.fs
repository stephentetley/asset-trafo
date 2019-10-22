// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocPatch

[<AutoOpen>]
module FuncLocPathType =

    open AssetPatch.Base
    open AssetPatch.Base.Syntax

    [<Struct>]
    [<CustomComparison; CustomEquality>]
    type FuncLocPath = 
        /// Store in reverse order so adding is cheap 
        /// (in reality the paths will be so short this doesn't matter)
        internal | FuncLocPath of string list

        member inline private x.Elements
            with get () : string list = let (FuncLocPath xs ) = x in xs
                


        override x.ToString() = 
            let (FuncLocPath xs) = x in String.concat "-" (List.rev xs)

        override x.Equals(yobj) = 
            match yobj with
            | :? FuncLocPath as y -> x.Elements = y.Elements
            | _ -> false

        override x.GetHashCode () = hash x.Elements

        member x.Level 
            with get () : int = x.Elements.Length
        
        
        interface System.IEquatable<FuncLocPath> with
            member x.Equals(y) = 
                x.Elements = y.Elements

        interface System.IComparable with
            member x.CompareTo(yobj) = 
                match yobj with
                | :? FuncLocPath as y -> 
                    match compare x.Level y.Level with 
                    | 0 -> x.ToString().CompareTo(y.ToString()) 
                    | x -> x
                | _ -> failwith "Invalid CompareTo on FlocPath"

        member x.Parent 
            with get (): FuncLocPath option = 
                let (FuncLocPath xs) = x
                match xs with
                | [] -> None
                | _ :: rest -> FuncLocPath rest |> Some

        static member Create (rootPath : string) : FuncLocPath = 
            let xs = rootPath.Split [| '-' |] |> List.ofArray |> List.rev
            FuncLocPath(xs)


[<RequireQualifiedAccess>]
module FuncLocPath =

    let extend (itemCode : string) (path : FuncLocPath) : FuncLocPath = 
        let (FuncLocPath xs)  = path
        FuncLocPath (itemCode :: xs)

    







    