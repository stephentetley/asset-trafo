// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module MarkdownOutput =

    
    open MarkdownDoc.Markdown
    
    open AssetTrafo.Aib.TreeDiff
    open AssetTrafo.Aib.StructureRelationsSimple


    let mdListNode (body : NodeBody) 
                   (kids : ParaElement list) : ParaElement = 
        let makeLabel (nb : NodeBody) : ParaElement = 
            paraText 
                <| rawtext "<a class=\"yellownode\">" 
                        ^^ text nb.Reference ^+^ text nb.Name 
                        ^^ rawtext"</a>"

        match kids with
        | [] -> makeLabel body
        | _ -> makeLabel body ^!^ unorderedList kids
        

    let renderAibTreeMarkdown (aibTree : AibTree) : Markdown = 
        let makeDoc (body : ParaElement) : Markdown = 
            h1 (text "Tree") ^!!^ markdown body
        let rec work (aib : AibTree) cont = 
            match aib with
            | Node(a, []) -> cont (mdListNode a [])
            | Node(a, kids) -> 
                workList kids ( fun xs -> 
                cont (mdListNode a xs))
        and workList kids cont = 
            match kids with 
            | [] -> cont []
            | x :: xs -> 
                work x (fun v1 -> 
                workList xs (fun vs -> 
                cont (v1 :: vs)))
        work aibTree (fun x -> x) 
            |> (fun x -> unorderedList [x])
            |> makeDoc