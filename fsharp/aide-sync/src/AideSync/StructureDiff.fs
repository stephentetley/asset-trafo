// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module StructureDiff =
    
    open System.Text

    open AideSync.Datatypes2


    
   

    

    /// F# design guidelines say favour object-interfaces rather than records of functions..

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

    let private parentOfPath (s : string) : string = 
        let parts = s.Split([| '?' |])
        if parts.Length > 0 then
            String.concat "?" parts.[0 .. parts.Length - 2]
        else ""

    
 
    // In cps...
    let buildTree (items : FlocDiff list) : Hierarchy<FlocDiff> option = 
        let rec getSiblings (parent : FlocDiff) 
                            (siblings : Hierarchy<FlocDiff> list) 
                            (workList : FlocDiff list) 
                            cont = 
            match workList with
            | [] -> cont (List.rev siblings, [])
            | k1 :: rest -> 
                if parentOfPath k1.PathKey = parent.PathKey then
                    getSiblings k1 [] rest (fun (kids,rest2) -> 
                    let knode = HierarchyNode(k1, List.rev kids)
                    getSiblings parent (knode :: siblings) rest2 cont)
                    
                else
                    let knode = HierarchyNode(k1, [])
                    getSiblings parent (knode :: siblings) rest cont
        match items with
        | [] -> None
        | root :: kids -> 
            let siblings, _ = getSiblings root [] kids (fun x -> x) 
            Some (HierarchyNode(root, siblings))


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