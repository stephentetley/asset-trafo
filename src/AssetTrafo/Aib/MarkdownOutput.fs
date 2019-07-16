// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module MarkdownOutput =

    
    open MarkdownDoc.Markdown
    
    open AssetTrafo.Aib.TreeDiff
    open AssetTrafo.Aib.StructureRelationsSimple


    let listNode (name : string) (kids : ParaElement list) : ParaElement = 
        let makeLabel (name : string) : ParaElement = 
            paraText 
                <| rawtext "<a class=\"yellownode\">" ^^ text name ^^ rawtext"</a>"

        match kids with
        | [] -> makeLabel name
        | _ -> makeLabel name ^!^ unorderedList kids
        

    let renderAibTreeMarkdown (aibTree : AibTree) : Markdown = 
        let makeDoc (body : ParaElement) : Markdown = 
            h1 (text "Tree") ^!!^ markdown body
        let rec work aib cont = 
            match aib with
            | Node(a, []) -> cont (listNode a [])
            | Node(a, kids) -> 
                workList kids ( fun xs -> 
                cont (listNode a xs))
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