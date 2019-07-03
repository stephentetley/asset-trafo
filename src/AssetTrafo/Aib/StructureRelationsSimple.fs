// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module StructureRelationsSimple =

    open FSharp.Data
    

    type StructureRelationship =
        { ParentRef : string
          ChildRef : string
          Level : int
        }


    type RelationshipsExport = 
        CsvProvider< Schema = @"ParentRef(string),ChildRef(string),Level(int)"
                   , Sample = "SAI010101010,SAI02020202,1"
                   , HasHeaders = true >

    type RelationshipRow = RelationshipsExport.Row
    
    let readRelationshipsExport (path:string) : RelationshipRow list = 
        let table = RelationshipsExport.Load(uri = path)
        Seq.toList table.Rows 



    /// Represent the Structure as a RoseTree
    type AibTree = 
        | AibTree of nodeId : string * kids : AibTree list

        member x.NodeId 
            with get () : string = 
                match x with | AibTree(x,_) -> x

        member x.Kids 
            with get () : AibTree list = 
                match x with | AibTree(_, kids) -> kids

        member x.WriteJson () : JsonValue = 
            let rec work (node : AibTree) 
                         (cont : JsonValue -> JsonValue) = 
                workList node.Kids (fun kids -> 
                cont (JsonValue.Record [| ("ref",  JsonValue.String node.NodeId )
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
            work x (fun x -> x)


    let private findRoot (relations : RelationshipRow list) : string option = 
        match relations with
        | [] -> None
        | top :: _ -> Some top.ParentRef
        
    let private findKids (parentId : string ) 
                              (relations : RelationshipRow list) : string list = 
        let chooseProc (rel : RelationshipRow) = 
            if parentId = rel.ParentRef then Some rel.ChildRef else None
        List.choose chooseProc relations

    let buildStructureTree (relations : RelationshipRow list) : AibTree option = 
        let sorter (node : AibTree) : string = node.NodeId
        let rec work parentId (cont : AibTree -> AibTree) = 
            let kidRefs = findKids parentId relations
            match kidRefs with
            | [] -> cont <| AibTree(parentId, [])
            | _ -> workList kidRefs (fun kids -> 
                    let kids1 = List.sortBy sorter kids
                    cont (AibTree(parentId, kids1)))
        and workList kids (cont : AibTree list -> AibTree) = 
            match kids with
            | [] -> cont []
            | kidRef1 :: rest ->
                work kidRef1 (fun a1 -> 
                workList rest (fun acc ->
                cont (a1 :: acc)))
        match findRoot relations with
        | None -> None 
        | Some label -> work label (fun a -> a) |> Some
        