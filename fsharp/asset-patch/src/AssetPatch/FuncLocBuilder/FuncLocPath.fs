// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FuncLocBuilder

[<AutoOpen>]
module FuncLocPathType =

    open AssetPatch.Base
    open AssetPatch.Base.Syntax

    [<Struct>]
    type FuncLocPath = 
        /// Store in reverse order so adding is cheap 
        /// (in reality the paths will be so short this doesn't matter)
        internal | FuncLocPath of string list

        override x.ToString() = 
            let (FuncLocPath xs) = x in String.concat "-" (List.rev xs)

        member x.Level 
            with get () : int = let (FuncLocPath arr) = x in arr.Length

        static member Create (rootPath : string) : FuncLocPath = 
            let xs = rootPath.Split [| '-' |] |> List.ofArray |> List.rev
            FuncLocPath(xs)


[<RequireQualifiedAccess>]
module FuncLocPath =

    let extend (itemCode : string) (path : FuncLocPath) : FuncLocPath = 
        let (FuncLocPath xs)  = path
        FuncLocPath (itemCode :: xs)

    let parent (path: FuncLocPath) : FuncLocPath option = 
        let (FuncLocPath xs) = path
        match xs with
        | [] -> None
        | _ :: rest -> FuncLocPath rest |> Some







    