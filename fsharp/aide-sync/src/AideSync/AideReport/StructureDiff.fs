// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.AideReport


module StructureDiff =
    
    open System.Text

    open AideSync.AideReport.Datatypes



    /// Note - the inputs will be sorted before the differences
    /// are calculated. 
    /// We rely on the inputs being ordered for the algorithm 
    /// to work - figuratively a pointer is running through each 
    /// list, and one will pause if it gets ahead of the other.
    let diffLists (leftList : AiFlocNode list) 
                  (rightList : AideFlocNode list) : FlocDiff list =
        let refSortL (xs : AiFlocNode list) = 
            xs |> List.sortBy (fun x -> x.Reference)
        
        let refSortR (xs : AideFlocNode list) = 
            xs |> List.sortBy (fun x -> x.Reference)

        let pathSort (xs : FlocDiff list) = 
            xs |> List.sortBy (fun x -> x.PathKey)

        let rec work lefts rights cont = 
            match lefts,rights with
            | xs, [] -> cont (List.map InLeft xs)
            | [], ys -> cont (List.map InRight ys)
            | (x::xs, y::ys) ->                 
                match compare (x.Reference) (y.Reference) with
                | i when i = 0 -> 
                    work xs ys (fun ac -> 
                    cont (InBoth(x,y) :: ac))                   
                | i when i < 0 -> 
                    // x is not in (y::ys)                 
                    work xs rights (fun ac -> 
                    cont (InLeft x :: ac))
                | i when i > 0 -> 
                    // y is not in (x::xs)
                    work lefts ys (fun ac -> 
                    cont (InRight y :: ac))
                | i -> failwithf "differenceL - Weird (impossible) pattern failure: %i" i
        work (refSortL leftList) (refSortR rightList) (fun x -> x)
            |> pathSort 

    let showDiffs (printL : AiFlocNode -> string) 
                  (printR : AideFlocNode -> string)
                  (diffs : FlocDiff list) : string = 
        let sb = new StringBuilder ()
        let write1 (diff : FlocDiff) : unit = 
            match diff with
            | InLeft s -> sb.AppendLine ("-" + printL s) |> ignore
            | InRight s -> sb.AppendLine ("+" + printR s) |> ignore
            | InBoth (_,s2) -> sb.AppendLine (" " + printR s2) |> ignore
        List.iter write1 diffs
        sb.ToString()


    // ************************************************************************
    // Build the tree

    let isChild1 (parent : string) (child : string) : bool = 
        let onePlus () = 
            let arrParent = parent.Split([| '?' |])
            let arrChild = child.Split([| '?' |])
            arrParent.Length + 1 = arrChild.Length
        child.StartsWith(parent) && onePlus ()



    type TreeStack = 
        | ParentStack of stack : Hierarchy<FlocDiff> list
        
        member x.Height 
            with get () : int = 
                let (ParentStack xs) = x in xs.Length
        
        member x.Top 
            with get () : Hierarchy<FlocDiff> = 
                let (ParentStack xs) = x
                match xs with
                | top :: _ -> top
                | _ -> failwith "Top of empty"
    
        member x.IsChild1 (child : FlocDiff) : bool = 
            let (ParentStack xs) = x
            match xs with
            | HierarchyNode(label,_) :: _ -> isChild1 label.PathKey child.PathKey
            | _ -> false
    
        member x.Push(child : Hierarchy<FlocDiff>) : TreeStack = 
            let (ParentStack xs) = x
            ParentStack (child :: xs)
    
        member x.Pop() : TreeStack = 
            let (ParentStack xs) = x
            match xs with
            | top :: HierarchyNode(label, kids) :: rest -> 
                let top1 = HierarchyNode(label, kids @ [top])
                ParentStack (top1 :: rest)
            | _ -> ParentStack []
    
        member x.Flatten () : Hierarchy<FlocDiff> option = 
            let rec work (stk : TreeStack) cont = 
                if stk.Height > 1 then 
                    work (stk.Pop()) cont
                else
                    let (ParentStack xs) = stk
                    match xs with 
                    | [one] -> cont (Some one)
                    | _ -> cont None
            work x (fun x -> x)
            
    
        static member Create (root : Hierarchy<FlocDiff>) : TreeStack = 
            ParentStack [root]
    
    let buildTree (source : FlocDiff list) : Hierarchy<FlocDiff> option =
        let rec work (input : FlocDiff list) 
                     (acc : TreeStack) 
                     cont =
            match input with
            | [] -> cont acc []
            | k1 :: rest -> 
                if acc.IsChild1 k1 then
                    work rest (acc.Push(HierarchyNode(k1,[]))) cont
                else    
                    work input (acc.Pop()) cont
    
        match source with 
        | [] -> None
        | root :: rest -> 
            let stack = TreeStack.Create( HierarchyNode(root, []) )
            let tree1, _ = work rest stack (fun x y -> (x,y))
            tree1.Flatten()


    // ************************************************************************
    // Prune the tree

    let nodeHasChanges (source : Hierarchy<StructureNode>) : bool = 
        let rec work tree cont = 
            match tree with
            | HierarchyNode(body : StructureNode, kids) -> 
                if body.CanBePruned then
                    cont true
                else workList kids cont
        and workList kids cont =
            match kids with
            | [] -> cont false
            | k1 :: rest ->
                work k1 (fun v1 -> 
                if v1 then cont v1 else workList rest cont)
        work source (fun x -> x)

    let pruneTree (source : Hierarchy<StructureNode>) : Hierarchy<StructureNode> option =
        let rec work tree cont = 
            match tree with 
            | HierarchyNode(label : StructureNode, kids) -> 
                workList kids (fun kids1 -> 
                match kids1, label.CanBePruned with
                | [], true -> cont None
                | _, _ -> cont (Some (HierarchyNode(label,kids1))))
        and workList kids cont = 
            match kids with 
            | [] -> cont []
            | k1 :: rest -> 
                work k1 (fun v1 -> 
                workList rest (fun vs -> 
                match v1 with
                | Some v -> cont (v::vs)
                | None -> cont vs))

        work source (fun x -> x)