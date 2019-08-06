// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module StructureRelationsSimple =

    open FSharp.Data

    open SLFormat

    open TikZDoc.Base
    open TikZDoc.Base.Properties
    open TikZDoc.Extensions.Forest

    open AssetTrafo.Aib.TreeDiff
    

    type StructureRelationship =
        { ParentRef : string
          ChildRef : string
          Level : int
        }


    type RelationshipsExport = 
        CsvProvider< Schema = @"ParentRef(string),ChildRef(string),ChildName(string),Level(int)"
                   , Sample = "SAI010101010,SAI02020202,some process,1"
                   , HasHeaders = true >

    type RelationshipRow = RelationshipsExport.Row
    
    let readRelationshipsExport (path:string) : RelationshipRow list = 
        let table = RelationshipsExport.Load(uri = path)
        Seq.toList table.Rows 



    /// Represent the Structure as a RoseTree
    type NodeBody = 
        { Reference : string 
          Name : string }

    type AibTree = Tree<NodeBody>
        

    let aibTreeToJson (tree: AibTree) : JsonValue = 
        let rec work (node : AibTree) 
                        (cont : JsonValue -> JsonValue) = 
            workList node.Kids (fun kids -> 
            cont (JsonValue.Record [| ("ref",  JsonValue.String node.Label.Reference )
                                    ; ("name",  JsonValue.String node.Label.Name )
                                    ; ("kids", JsonValue.Array (List.toArray kids)) 
                                    |] ))
        and workList (kids : AibTree list) 
                        (cont : JsonValue list -> JsonValue)  = 
            match kids with
            | [] -> cont []
            | k1 :: rest -> 
                work k1 (fun v1 ->
                workList rest (fun acc ->
                cont (v1 :: acc)))
        work tree (fun x -> x)


    let private findRoot (relations : RelationshipRow list) : NodeBody option = 
        match relations with
        | [] -> None
        | top :: _ -> Some { Reference = top.ParentRef
                             Name = "TEMP" }
        
    let private findKids (parentId : string ) 
                              (relations : RelationshipRow list) : NodeBody list = 
        let chooseProc (rel : RelationshipRow) : NodeBody option= 
            if parentId = rel.ParentRef then 
                Some { Reference = rel.ChildRef 
                       Name = rel.ChildName }
            else None
        List.choose chooseProc relations

    let buildStructureTree (relations : RelationshipRow list) : AibTree option = 
        let sorter (node : AibTree) : string = node.Label.Reference
        let rec work parentNode (cont : AibTree -> AibTree) = 
            let kidRefs = findKids parentNode.Reference relations
            match kidRefs with
            | [] -> cont <| Node(parentNode, [])
            | _ -> workList kidRefs (fun kids -> 
                    let kids1 = List.sortBy sorter kids
                    cont (Node(parentNode, kids1)))
        and workList kids (cont : AibTree list -> AibTree) = 
            match kids with
            | [] -> cont []
            | kidRef1 :: rest ->
                work kidRef1 (fun a1 -> 
                workList rest (fun acc ->
                cont (a1 :: acc)))
        match findRoot relations with
        | None -> None 
        | Some node -> work node (fun a -> a) |> Some
        

    let loadStructureRelationships (path:string) : AibTree option = 
        readRelationshipsExport path 
            |> buildStructureTree

    let drawAibTree (aibTree : AibTree) : string = 
        let rec work aib cont = 
            match aib with
            | Node(a,[]) -> cont (RoseTree.RoseTree(a, []))
            | Node(a, kids) -> 
                workList kids ( fun xs -> 
                cont (RoseTree.RoseTree(a,xs)))
        and workList kids cont = 
            match kids with 
            | [] -> cont []
            | x :: xs -> 
                work x (fun v1 -> 
                workList xs (fun vs -> 
                cont (v1 :: vs)))
        work aibTree (fun x -> x) 
            |> RoseTree.mapTree (fun body -> sprintf "%s %s" body.Reference body.Name)
            |> RoseTree.drawTree

        
    let renderAibTreeTikZ (aibTree : AibTree) : LaTeX = 
        let makeDoc body = 
            forestDocument ["edges" ]
                <| vcat [ forTree [growTick <| latexInt 0; folderProp; drawProp]
                        ; body ]
        let rec work (aib : AibTree) cont = 
            match aib with
            | Node(a,[]) -> cont (forestNode (text a.Reference) [])
            | Node(a, kids) -> 
                workList kids ( fun xs -> 
                cont (forestNode (text a.Reference) xs))
        and workList kids cont = 
            match kids with 
            | [] -> cont []
            | x :: xs -> 
                work x (fun v1 -> 
                workList xs (fun vs -> 
                cont (v1 :: vs)))
        work aibTree (fun x -> x) |> makeDoc 
