﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.AideReport


module Datatypes =
    
    open MarkdownDoc.Markdown.RoseTree

    open AideSync.AideReport.Attributes


    /// Assets have special attributes (Installed from date, 
    /// Location Ref) that we call Properties

    
    

 
    type AiFlocNode = 
        { AssetId : int64
          Reference : string
          ShortName : string 
          CommonName : string
          ParentReference : string }

        // The SortKey is not reliable for building.
        // We want to be able to sort top down (priority go down, 
        // rather than go next).
        // The simplest way to do this is hack the key so '/' has 
        // priority over ' '.
        member v.SortKey 
            with get() : string  = 
                v.CommonName.Replace(' ', '?')

     type AideFlocNode = 
         { AideAssetId : int64        
           Reference : string
           ShortName : string 
           CommonName : string
           ParentReference : string }
         
         member v.SortKey 
             with get() : string  = 
                 v.CommonName.Replace(' ', '?')



    /// A StructureDiff is a difference between the item in the Ai and Aide 
    /// hierarchies.
    type FlocDiff = 
        | InLeft of AiFlocNode
        | InBoth of AiFlocNode * AideFlocNode
        | InRight of AideFlocNode

        /// To output we want an almost lexigraphical order but 
        /// with '/' favoured over ' '
        member v.SortKey 
            with get() : string  = 
                match v with
                | InLeft s -> s.SortKey
                | InBoth (s1,_) -> s1.SortKey
                | InRight s -> s.SortKey

        member v.Reference 
            with get() : string  = 
                match v with
                | InLeft s -> s.Reference
                | InBoth (s1,_) -> s1.Reference
                | InRight s -> s.Reference

        member v.ParentReference 
            with get() : string  = 
                match v with
                | InLeft s -> s.ParentReference
                | InBoth (s1,_) -> s1.ParentReference
                | InRight s -> s.ParentReference


    type StructureNode = 
        | Deleted of node : AiFlocNode 
        | Common of AiFlocNode * AideFlocNode * NodeChanges
        | Added of AideFlocNode * NodeChanges

        member x.CanBePruned 
            with get () : bool = 
                match x with
                | Common(_,_,changes) -> not changes.HasChanges
                | _ -> false


        member x.ShortName 
            with get () : string = 
                match x with
                | Deleted node -> node.ShortName
                | Common(node, _, _) -> node.ShortName
                | Added(node, _) -> node.ShortName

        member x.CommonName 
            with get () : string = 
                match x with
                | Deleted node -> node.CommonName
                | Common(node, _, _) -> node.CommonName
                | Added(node, _) -> node.CommonName
                
        member x.Reference
            with get () : string = 
                match x with
                | Deleted node -> node.Reference
                | Common(node, _, _) -> node.Reference
                | Added(node, _) -> node.Reference

        member x.AttributeChanges 
            with get () : NodeChanges option = 
                match x with
                | Deleted _ -> None
                | Common(_, _, changes) -> Some changes
                | Added(_, changes) -> Some changes

    /// Hierarchy is polymorphic on label.
    /// The label can represent an elementary item or a delta between
    /// nodes in an Ai tree and an Aide tree.
    type Hierarchy<'Label> = 
        | HierarchyNode of label : 'Label * kids : Hierarchy<'Label> list


        member x.RootLabel 
            with get () : 'Label = 
                match x with
                | HierarchyNode(label,_) -> label

        member x.ToMarkdownTree () : RoseTree<'Label> = 
            let rec work tree cont = 
                match tree with
                | HierarchyNode(label, kids) -> 
                    workList kids (fun vs -> 
                    cont (Node(label,vs)))
            and workList kids cont = 
                match kids with 
                | [] -> cont []
                | k1 :: rest -> 
                    work k1 (fun v1 -> 
                    workList rest (fun vs -> 
                    cont (v1 :: vs)))
            work x (fun x -> x)
  
    let collectChanges (sourceTree : Hierarchy<StructureNode>) : (string * NodeChanges) list = 
        let rec work tree cont = 
            match tree with
            | HierarchyNode(label : StructureNode,kids) -> 
                let changes1 = 
                    label.AttributeChanges |> Option.map (fun x -> label.Reference, x)
                workList kids (fun vs ->
                cont (changes1 :: vs))
        and workList kids cont = 
            match kids with
            | [] -> cont []
            | k1 :: rest -> 
                work k1 (fun xs -> 
                workList rest (fun ys -> 
                cont (xs @ ys)))
        work sourceTree (fun x -> x) |> List.choose id



    type ChangeRequestInfo = 
        { ChangeRequestId : int64
          RequestType : string
          Status : string
          Comment : string
          RequestTime : System.DateTime
        }


    /// Potentially there may be multiple change requests
    /// with the same ChangeRequestInfo header
    type ChangeRequest = 
        { Info : ChangeRequestInfo 
          StructureChange : Hierarchy<StructureNode> }

    type ChangeSchemeInfo = 
        { SchemeId : int64
          SchemeCode : string
          Name : string
          SolutionProvider : string
        }

    type ChangeScheme = 
        { Info : ChangeSchemeInfo
          ChangeRequests : ChangeRequest list
        }
