// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
open System
open System.Text


#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20190903\lib\netstandard2.0"
#r "MarkdownDoc.dll"
open MarkdownDoc.Markdown
open MarkdownDoc.Markdown.RoseTree

let sampleInput = 
    [ "a"
      "a/b1"
      "a/b1/c1"
      "a/b1/c2"
      "a/b2"
      "a/b2/c3"
      "a/b2/c4"
      "a/b3"
      "a/b3/c5"
    ]

let isChild1 (parent : string) (child : string) : bool = 
    let onePlus () = 
        let arrParent = parent.Split([| '/' |])
        let arrChild = child.Split([| '/' |])
        arrParent.Length + 1 = arrChild.Length
    child.StartsWith(parent) && onePlus ()

let label (s : string) : Markdown = s |> rawtext |> markdownText

let leaf (s : string) : RoseTree<string> = 
    Node(s, [])



type TreeStack = 
    | ParentStack of stack : RoseTree<string> list
    
    member x.Height 
        with get () : int = 
            let (ParentStack xs) = x in xs.Length


    member x.Top 
        with get () : RoseTree<string> = 
            let (ParentStack xs) = x
            match xs with
            | top :: _ -> top
            | _ -> failwith "Top of empty"

    member x.IsChild1 (child : string) : bool = 
        let (ParentStack xs) = x
        match xs with
        | Node(label,_) :: _ -> isChild1 label child
        | _ -> false

    member x.Push(child : RoseTree<string>) : TreeStack = 
        let (ParentStack xs) = x
        ParentStack (child :: xs)

    member x.Pop() : TreeStack = 
        let (ParentStack xs) = x
        match xs with
        | top :: Node(label, kids) :: rest -> 
            let top1 = Node(label, kids @ [top])
            ParentStack (top1 :: rest)
        | _ -> ParentStack []

    member x.Flatten () : RoseTree<String> option = 
        let rec work (stk : TreeStack) cont = 
            if stk.Height > 1 then 
                work (stk.Pop()) cont
            else
                let (ParentStack xs) = stk
                match xs with 
                | [one] -> cont (Some one)
                | _ -> cont None
        work x (fun x -> x)
        

    static member Create (root : RoseTree<string>) : TreeStack = 
        ParentStack [root]


let buildTree (source : string list) : RoseTree<string> option =
    let rec work (input : string list) 
                 (acc : TreeStack) 
                 cont =
        match input with
        | [] -> cont acc []
        | k1 :: rest -> 
            if acc.IsChild1 k1 then
                work rest (acc.Push(Node(k1,[]))) cont
            else    
                work input (acc.Pop()) cont

    match source with 
    | [] -> None
    | root :: rest -> 
        let stack = TreeStack.Create( RoseTree.Node(root, []) )
        let tree1, _ = work rest stack (fun x y -> (x,y))
        tree1.Flatten()

let outputTreeOpt (source : RoseTree<string> option) : unit = 
    match source with
    | None -> printfn "None"
    | Some tree -> 
        tree|> RoseTree.mapTree label 
        |> RoseTree.drawTree |> renderMarkdown 400 |> printfn "%s"
    

let test01 () = 
    buildTree sampleInput |> outputTreeOpt
    

let test02 () = 
    TreeStack.Create(leaf "A").Push(leaf "B").Push(leaf "C").Flatten()
        |> outputTreeOpt
