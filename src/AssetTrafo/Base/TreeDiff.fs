// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

// Acknowledgment
// ==============
// This is an implementation of the tree diff and patch code
// from "Type-safe diff for families of datatypes" by Eelco
// Lempsink, Sean Leather and Andres Löh. 
// The code has been CPS transformed.

/// NOTE - this file is copied from SLAlignment.
/// We need to use a package dependency on sl-alignment at some point.

namespace AssetTrafo.Aib


module TreeDiff = 

    type Tree<'a> = 
        | Node of label : 'a * kids : Tree<'a> list

        member x.Label 
            with get () : 'a = 
                match x with | Node(x,_) -> x

        member x.Kids 
            with get () : Tree<'a> list = 
                match x with | Node(_, kids) -> kids

    type Diff<'a> = 
        | Ins of 'a * arity : int
        | Del of 'a * arity : int
        | Cpy of 'a * arity : int
   
    let private insertCps (x:'a) 
                  (arity:int) 
                  (trees : Tree<'a> list) 
                  (fk : unit -> (Tree<'a> list) option)
                  (sk : Tree<'a> list -> (Tree<'a> list) option) : (Tree<'a> list) option = 
        try 
            let (ys, yss) = List.splitAt arity trees
            sk (Node(x,ys) :: yss)
        with
        | _ -> fk ()

    
    let private deleteCps (x:'a) 
                  (arity:int) 
                  (trees : Tree<'a> list)
                  (fk : unit -> (Tree<'a> list) option)
                  (sk : Tree<'a> list -> (Tree<'a> list) option) : (Tree<'a> list) option = 
        match trees with
        | [] -> fk ()
        | Node(y, ys) :: yss -> 
            if x = y && arity = ys.Length then sk (ys @ yss) else fk ()


    let patch (patchSteps : Diff<'a> list) (source : Tree<'a> list) : (Tree<'a> list) option = 
        let rec work ps xs fk sk = 
            match ps, xs with
            | Ins(x, sz) :: ds, ys -> 
                work ds ys fk (fun ac ->
                insertCps x sz ac fk sk)

            | Del(x, sz) :: ds, ys -> 
                deleteCps x sz ys fk (fun ac ->
                work ds ac fk sk)

            | Cpy(x, sz) :: ds, ys ->
                deleteCps x sz ys fk (fun ac1 ->
                work ds ac1 fk (fun ac2 -> 
                insertCps x sz ac2 fk sk))

            | [], [] -> sk []

            | [], _ -> fk ()
        work patchSteps source (fun _ -> None) (fun xs -> Some xs)


    let inline private cost (diff: Diff<'a> list) : int = diff.Length

    let inline private choose (dx : Diff<'a> list) (dy : Diff<'a> list) : Diff<'a> list = 
        if cost dx <= cost dy then dx else dy

    let hedgeDiff (list1 : Tree<'a> list) (list2 : Tree<'a> list) : Diff<'a> list = 
        let rec work ks ls cont = 
            match ks,ls with
            | [], [] -> cont []
            | [], (Node(y,ys) :: yss) -> 
                work [] (ys @ yss) (fun ac ->
                cont (Ins(y, ys.Length) :: ac))

            | (Node(x,xs) :: xss), [] -> 
                work (xs @xss) [] (fun ac ->
                cont (Del(x, xs.Length) :: ac))

            | (Node(x,xs) :: xss), (Node(y,ys) :: yss) ->
                if x = y && xs.Length = ys.Length then best3 x xs xss y ys yss cont else best2 x xs xss y ys yss cont

        and best2 x xs xss y ys yss cont =
            work (xs @ xss) (Node(y,ys) :: yss) (fun acD ->
            work (Node(x,xs) :: xss) (ys @ yss) (fun acI -> 
            cont (choose (Del(x, xs.Length) :: acD) (Ins(y, ys.Length) :: acI))))

        and best3 x xs xss y ys yss cont =
            work xs ys (fun acC ->
            best2 x xs xss y ys yss (fun acB2 ->
            cont (choose (Cpy(x, xs.Length) :: acC) acB2)))

        work list1 list2 (fun xs -> xs)

    let treeDiff (tree1 : Tree<'a>) (tree2 : Tree<'a>) : Diff<'a> list = 
        hedgeDiff [tree1] [tree2]