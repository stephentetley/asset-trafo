// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module Datatypes2 =
    
    open AideSync.Attributes


    /// Assets have special attributes (Installed from date, 
    /// Location Ref) that we call Properties

    
    type NameValueDiff =
        | OnlyLeft of name : string * value : string
        | Difference of name : string * leftValue : string * rightValue : string
        | OnlyRight of name : string * value : string

 
    type AiFlocNode = 
        { AssetId : int64
          Reference : string
          ShortName : string 
          CommonName : string }
        // We want to be able to sort top down (priority go down, 
        // rather than go next).
        // The simplest way to do this is hack the key so '/' has 
        // priority over ' '.
        member v.PathKey 
            with get() : string  = 
                v.CommonName.Replace(' ', '?')

     type AideFlocNode = 
         { AideAssetId : int64        
           Reference : string
           ShortName : string 
           CommonName : string }
         // We want to be able to sort top down (priority go down, 
         // rather than go next).
         // The simplest way to do this is hack the key so '/' has 
         // priority over ' '.
         member v.PathKey 
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
        member v.PathKey 
            with get() : string  = 
                match v with
                | InLeft s -> s.PathKey
                | InBoth (s1,_) -> s1.PathKey
                | InRight s -> s.PathKey



    type StructureNode = 
        | Deleted of node : AiFlocNode 
        | Common of AiFlocNode * AideFlocNode * NodeChanges
        | Added of AideFlocNode * NodeChanges

        member x.CanBePruned 
            with get () : bool = 
                match x with
                | Common(_,_,changes) -> not changes.HasChanges
                | _ -> false


    /// Hierarchy is polymorphic on label.
    /// The label can represent an elementary item or a delta between
    /// nodes in an Ai tree and an Aide tree.
    type Hierarchy<'Label> = 
        | HierarchyNode of label : 'Label * kids : Hierarchy<'Label> list

  

    type ChangeRequestInfo = 
        { ChangeRequestId : int64
          RequestType : string
          Status : string
          Comment : string
          RequestTime : System.DateTime
        }

    type ChangeRequest = 
        { Info : ChangeRequestInfo 
          Changes : Hierarchy<StructureNode> list }

    type ChangeSchemeInfo = 
        { SchemeId : int64
          SchemeCode : string
          Name : string
          SolutionProvider : string
        }

    type ChangeScheme = 
        { Info : ChangeSchemeInfo
          StructureChanges : ChangeRequest list
        }
