﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.AideReport.Internal


module Datatypes =
    
    open MarkdownDoc.Markdown.RoseTree

    open AideSync.AideReport.Internal.Attributes


    /// Assets have special attributes (Installed from date, 
    /// Location Ref) that we call Properties

    
    

 
    type AiFlocNode = 
        { AssetId : int64
          Reference : string
          HierarchyLevel : int
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
           HierarchyLevel : int
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
                | InBoth (_,s2) -> s2.SortKey
                | InRight s -> s.SortKey

        /// This favours the Aide CommonName
        member v.CommonName 
            with get() : string  = 
                match v with
                | InLeft s -> s.CommonName
                | InBoth (_,s2) -> s2.CommonName
                | InRight s -> s.CommonName

        member v.Reference 
            with get() : string  = 
                match v with
                | InLeft s -> s.Reference
                | InBoth (_,s2) -> s2.Reference
                | InRight s -> s.Reference

        member v.ParentReference 
            with get() : string  = 
                match v with
                | InLeft s -> s.ParentReference
                | InBoth (_,s2) -> s2.ParentReference
                | InRight s -> s.ParentReference

        member v.HierarchyLevel
            with get() : int  = 
                match v with
                | InLeft s -> s.HierarchyLevel
                | InBoth (_,s2) -> s2.HierarchyLevel
                | InRight s -> s.HierarchyLevel


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

        member x.ValueChanges 
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
  
        member x.Flatten () : 'Label list = 
            let rec work tree cont = 
                match tree with
                | HierarchyNode(label,kids) -> 
                    workList kids (fun vs ->
                    cont (label :: vs))
            and workList kids cont = 
                match kids with
                | [] -> cont []
                | k1 :: rest -> 
                    work k1 (fun xs -> 
                    workList rest (fun ys -> 
                    cont (xs @ ys)))
            work x (fun xs -> xs) 

        member x.Map (mapper : 'Label -> 'NewLabel) : Hierarchy<'NewLabel> = 
            let rec work tree cont = 
                match tree with
                | HierarchyNode(label,kids) ->
                    let label1 = mapper label
                    workList kids (fun kids1 ->
                    cont (HierarchyNode(label1, kids1)))
            and workList kids cont = 
                match kids with
                | [] -> cont []
                | k1 :: rest -> 
                    work k1 (fun kid1 -> 
                    workList rest (fun kids2 -> 
                    cont (kid1 :: kids2)))
            work x (fun a -> a) 

    let hierarchyMap (mapper : 'a -> 'b) 
                     (tree : Hierarchy<'a>) : Hierarchy<'b> = 
        tree.Map(mapper)

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
          BatchReference : string option
          BrmSolutionId : string option
          BrmCompletionYear : int option
        }

    type ChangeScheme = 
        { Info : ChangeSchemeInfo
          ChangeRequests : ChangeRequest list
        }
