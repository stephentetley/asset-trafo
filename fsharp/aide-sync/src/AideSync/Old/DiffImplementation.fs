// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.Old


module DiffImplementation =
    
    open System.Text

    open MarkdownDoc.Markdown
    open MarkdownDoc.Markdown.InlineHtml
    open MarkdownDoc.Markdown.CssColors
    open MarkdownDoc.Markdown.RoseTree

    open AideSync.Base.Addendum
    open AideSync.Old.Datatypes

    



    /// F# design guidelines say favour object-interfaces rather than records of functions..

    /// Note - the inputs will be sorted before the differences
    /// are calculated. 
    /// We rely on the inputs being ordered for the algorithm 
    /// to work - figuratively a pointer is running through each 
    /// list, and one will pause if it gets ahead of the other.
    let diffLists (leftList : AiStructureItem list) 
                  (rightList : AideStructureItem list) : Differences =
        let refSortL (xs : AiStructureItem list) = 
            xs |> List.sortBy (fun x -> x.Reference)
        
        let refSortR (xs : AideStructureItem list) = 
            xs |> List.sortBy (fun x -> x.Reference)

        let pathSort (xs : StructureItemDiff list) = 
            xs |> List.sortBy (fun x -> x.PathKey)

        let rec work lefts rights cont = 
            match lefts,rights with
            | xs, [] -> cont (List.map InLeft xs)
            | [], ys -> cont (List.map InRight ys)
            | (x::xs, y::ys) ->                 
                match compare (x.Reference) (y.Reference) with
                | i when i = 0 -> 
                    work xs ys (fun ac -> 

                    // It is possible to have nonproper a name change that
                    // tries to change the prefix path to the current node
                    // but don't change the names of the nodes above it.
                    // This reults in the leaf being highlighted even though 
                    // the "change" is above it.
                    if x.CommonName = y.CommonName then 
                        cont (Match(x,y) :: ac)
                    else
                        cont (Difference(x,y) :: ac))

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

    let showDiffs (printL : AiStructureItem -> string) 
                  (printR : AideStructureItem -> string)
                  (diffs : Differences) : string = 
        let sb = new StringBuilder ()
        let write1 (diff : StructureItemDiff) : unit = 
            match diff with
            | InLeft s -> sb.AppendLine ("-" + printL s) |> ignore
            | InRight s -> sb.AppendLine ("+" + printR s) |> ignore
            | Match (_,s2) -> sb.AppendLine (" " + printR s2) |> ignore
            
            | Difference(s1,s2) -> 
                sb.AppendLine (sprintf "*[%s]=>%s" (printL s1) (printR s2)) |> ignore
        List.iter write1 diffs
        sb.ToString()

    // ************************************************************************
    // Render to Markdown 

    type TreeItem = { Path : string ; Difference : StructureItemDiff}


    let pathLength (path : string) : int = 
        match path.Split(separator=[| '/' |]).Length with
        | x when x > 1 -> x-1
        | x -> x
        
    let isDirectChildPath (parent : string) (child : string) = 
        child.StartsWith(parent) && pathLength child = pathLength parent + 1
        
    

    let buildStructureTree (diffs : StructureItemDiff list) : RoseTree<TreeItem> option = 
        printfn "Length of diffs = %i" diffs.Length
        let makeNode (d1 : StructureItemDiff) : RoseTree<TreeItem> = 
            makeLeaf { Path = d1.PathKey; Difference = d1} 

        /// Parent exists!
        let addNode (d1 : StructureItemDiff) 
                    (tree1 : RoseTree<TreeItem>) : RoseTree<TreeItem> =
            let rec work (t1 : RoseTree<TreeItem>) cont =
                match t1 with
                | Node(label,kids) -> 
                    if isDirectChildPath label.Path d1.PathKey then 
                        cont (Node(label, kids @ [makeNode d1]))
                    else
                        workList kids (fun kids1 ->
                        cont (Node(label,kids1)))
            and workList (xs : RoseTree<TreeItem> list) cont = 
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

    
    let span (colourName : string) (extraAttrs : HtmlAttrs) (body : Text) : Text = 
        htmlSpan (attrStyle [backgroundColor colourName] :: extraAttrs) body

    let drawLabel (isRoot : bool) (item : TreeItem)  : Markdown = 
        let makeLabelL (item : AiStructureItem) : Text = 
            text <| if isRoot then item.CommonName else item.Name 
        let makeLabelR (item : AideStructureItem) : Text = 
            text <| if isRoot then item.CommonName else item.Name

        match item.Difference with
        | InLeft s -> 
            let title = htmlAttr "title" (sprintf "Delete '%s'" s.CommonName)
            span lightCoral [title] (makeLabelL s) |> markdownText
        | Match(_,s) -> makeLabelR s |> markdownText
        | Difference (s1,s2) -> 
            let title = 
                if s1.Name <> s2.Name then 
                    htmlAttr "title" (sprintf "Rename '%s' to '%s'" s1.Name s2.Name)
                else
                    htmlAttr "title" (sprintf "Non-proper name change (floc path editted):&#013;'%s'&#013;to&#013;'%s'" s1.CommonName s2.CommonName)
            span gold [title] (makeLabelR s2)  |> markdownText
        | InRight s -> 
            let title = htmlAttr "title" (sprintf "Add '%s'" s.CommonName)
            span paleGreen [title] (makeLabelR s) |> markdownText

    let drawStructure (diffs : Differences) : Markdown = 
        match buildStructureTree diffs with
        | None -> emptyMarkdown
        | Some tree -> 
            mapTree2 (drawLabel true) (drawLabel false) tree |> drawTree

    // ************************************************************************
    // Prune tree


    let pruneTree (source : RoseTree<TreeItem>) : RoseTree<TreeItem> option = 
        let rec hasDiff (Node(label,kids):RoseTree<TreeItem>) cont = 
            if label.Difference.HasDiff then 
                cont true
            else
                listHasDiff kids cont
        and listHasDiff kids cont = 
            match kids with
            | [] -> cont false
            | x :: xs -> 
                hasDiff x (fun v1 ->
                if v1 then 
                    cont v1 
                else
                    listHasDiff xs cont)


        let rec work (Node(label,kids):RoseTree<TreeItem>) cont = 
            workList kids (fun (vs : RoseTree<TreeItem> list) -> 
            if (not <| vs.IsEmpty) || label.Difference.HasDiff then 
                cont (Some (Node(label,vs)))
            else
                cont None)              

        and workList kids cont = 
            match kids with
            | [] -> cont []
            | x :: xs -> 
                work x ( fun v1 -> 
                workList xs (fun vs -> 
                let results = 
                    match v1 with
                    | None -> vs
                    | Some v -> v :: vs
                cont results ))
        work source (fun x -> x)


    let drawPrunedStructure (diffs : Differences) : Markdown = 
        match buildStructureTree diffs with
        | None -> emptyMarkdown
        | Some tree -> 
            match tree |> pruneTree with
            | None -> text "No changes" |> doubleAsterisks |> markdownText
            | Some tree -> 
                tree |> mapTree2 (drawLabel true) (drawLabel false) |> drawTree