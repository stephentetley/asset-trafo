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

        /// Note - we need to see parent before we can add child
        /// Sorting on level should be good enough
        let levelSort (xs : FlocDiff list) = 
            xs |> List.sortBy (fun x -> x.HierarchyLevel)

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
            |> levelSort 

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


    // Note 
    // ====
    // The ``FlocDiff list`` is reliably sorted if we sort on HierarchyLevel.
    
        

    // ``buildTree`` has a very strong precodition that the input list
    // must be sorted so parent nodes are before child nodes.
    // It will lose data (nodes) if this is not the case.
    let buildTree (diffs : FlocDiff list) : Hierarchy<FlocDiff> option = 
        printfn "Length of diffs = %i" diffs.Length
        let makeNode (d1 : FlocDiff) : Hierarchy<FlocDiff> = 
            HierarchyNode(d1,[])

        /// Parent exists!
        let addNode (d1 : FlocDiff) 
                    (tree1 : Hierarchy<FlocDiff>) : Hierarchy<FlocDiff> =
            let rec work (t1 : Hierarchy<FlocDiff>) cont =
                match t1 with
                | HierarchyNode(label,kids) -> 
                    if label.Reference = d1.ParentReference then 
                        cont (HierarchyNode(label, kids @ [makeNode d1]))
                    else
                        workList kids (fun kids1 ->
                        cont (HierarchyNode(label,kids1)))
            and workList (xs : Hierarchy<FlocDiff> list) cont = 
                match xs with
                | [] -> cont []     /// really a failure but should be impossible
                | x :: rest -> 
                    work x (fun v1 -> 
                        if x = v1 then
                            workList rest (fun vs -> 
                            cont (v1 :: vs))
                         else 
                            cont (v1 :: rest))
            work tree1 (fun x -> x)
        
        match diffs with
        | [] -> None 
        | root :: rest -> 
            List.fold (fun st a -> addNode a st) (makeNode root) rest |> Some



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