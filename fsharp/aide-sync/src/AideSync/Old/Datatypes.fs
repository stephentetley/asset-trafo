// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.Old


module Datatypes =
    
    
    /// Uid will be either AiAssetId (if item drawn from AI)
    /// or AideAssetId (if drawn from Aide).
    type AiStructureItem = 
        { AssetId : int64
          Name: string 
          CommonName : string 
          Reference : string
        }
        // We want to be able to sort top down (priority go down, 
        // rather than go next).
        // The simplest way to do this is hack the key so '/' has 
        // priority over ' '.
        member v.PathKey 
            with get() : string  = 
                v.CommonName.Replace(' ', '?')

    type AideStructureItem = 
        { AideAssetId : int64
          Name: string 
          CommonName : string 
          Reference : string
        }
        // We want to be able to sort top down (priority go down, 
        // rather than go next).
        // The simplest way to do this is hack the key so '/' has 
        // priority over ' '.
        member v.PathKey 
            with get() : string  = 
                v.CommonName.Replace(' ', '?')


    /// A StructureItemDiff is a difference between the item in the Ai and Aide 
    /// hierarchies.
    type StructureItemDiff = 
        | InLeft of AiStructureItem
        | Match of left : AiStructureItem * right : AideStructureItem
        | Difference of left : AiStructureItem * right : AideStructureItem
        | InRight of AideStructureItem

        member v.HasDiff 
            with get () : bool =
                match v with
                | Match _ -> false
                | _ -> true

        /// To output we want an almost lexigraphical order but 
        /// with '/' favoured over ' '
        member v.PathKey 
            with get() : string  = 
                match v with
                | InLeft s -> s.PathKey
                | Match (s1,_) -> s1.PathKey
                | Difference (s1,_) -> s1.PathKey
                | InRight s -> s.PathKey


    type Differences = StructureItemDiff list


    let aideIdsOfDifferences (diffs : Differences) : int64 list = 
        let select (diff : StructureItemDiff) : int64 option = 
            match diff with
            | InLeft _ -> None
            | Match(_,_) -> None
            | Difference (_,s2) -> Some s2.AideAssetId
            | InRight s -> Some s.AideAssetId
        List.choose select diffs
 
    type AiHierarchy = 
        val private StructureItems : AiStructureItem list

        new (items : AiStructureItem list) = 
            let fn (s1 : AiStructureItem) = s1.CommonName
            { StructureItems = List.sortBy fn items }

        member x.Size 
            with get () : int = x.StructureItems.Length


        member x.Items 
            with get () : AiStructureItem list = x.StructureItems

        member x.CommonNames 
            with get () : string list = 
                x.StructureItems |> List.map (fun x -> x.CommonName)

        member x.References 
            with get () : string list = 
                x.StructureItems |> List.map (fun x -> x.Reference)


    type AideHierarchy = 
        val private StructureItems : AideStructureItem list

        new (items : AideStructureItem list) = 
            let fn (s1 : AideStructureItem) = s1.CommonName
            { StructureItems = List.sortBy fn items }

        member x.Size 
            with get () : int = x.StructureItems.Length


        member x.Items 
            with get () : AideStructureItem list = x.StructureItems

        member x.CommonNames 
            with get () : string list = 
                x.StructureItems |> List.map (fun x -> x.CommonName)

        member x.References 
            with get () : string list = 
                x.StructureItems |> List.map (fun x -> x.Reference)

    type AttributeValue = 
        | Literal of string
        | Lookup of resolved : string
        member x.Value 
            with get() : string = 
                match x with
                | Literal s -> s
                | Lookup s -> s

        
    /// This represents a possible change on an attribute.
    /// The report on the system that we use for input only 
    /// shows changes. If an attribute is unchanged there will 
    /// be no record.
    type AttributeDelta = 
        { AttributeName : string
          AiValue : AttributeValue
          AideValue : AttributeValue
        }
        /// Note this should always be true as we only extract changed 
        /// records from the master database
        member x.HasChanged 
            with get () : bool = x.AideValue <> x.AiValue

    type RepeatedAttributeDelta = 
        { RepeatedAttributeName : string
          AttributeSetName : string
          AiValue : AttributeValue
          AideValue : AttributeValue
        } 
        /// Note this should always be true as we only extract changed 
        /// records from the master database
        member x.HasChanged 
            with get () : bool = x.AideValue <> x.AiValue

    /// Assets have "properties" { name, common name, manufacturer, 
    /// grid ref... } that are stored inline with the "Asset record". 
    /// Because they are stored inline, "properties" are diffferent
    /// to attributes that are stored in a separate table with a foreign
    /// key back to the asset.
    /// The input report lists both AI and AIDE property values regardless
    /// of whether the value has changed in AIDE.
    type AssetPropertyDelta = 
        { PropertyName : string
          AiValue : string
          AideValue : string
        }
        member x.HasChanged 
            with get () : bool = x.AideValue <> x.AiValue

    type AssetInfo = 
        { AssetId : int64 
          AssetReference : string
          AssetName : string
          CommonName : string
        }

    type AssetChangeset = 
        { AssetProperties : AssetPropertyDelta list
          AttrChanges : AttributeDelta list
          RepeatedAttrChanges : RepeatedAttributeDelta list
        }
        member x.HasChanged 
            with get () : bool = 
                x.AssetProperties |> List.exists (fun prop -> prop.HasChanged) 
                    || x.AttrChanges |> List.exists (fun attr -> attr.HasChanged) 
                    || x.RepeatedAttrChanges |> List.exists (fun attr -> attr.HasChanged) 

    type AssetChange = 
        { AssetInfo : AssetInfo
          AssetChanges : AssetChangeset
        }
        
    type AssetStructureChange = 
        { AssetInfo : AssetInfo
          StructureChanges : Differences
          KidsChanges : AssetChangeset list
        }

    type ChangeRequestInfo = 
        { ChangeRequestId : int64
          RequestType : string
          Status : string
          Comment : string
          RequestTime : System.DateTime
        }


    // Use this to prevent a tricky recursive hierarchy
    type AttributeChangeRequest = 
        { Info : ChangeRequestInfo 
          AssetChanges : AssetChange list 
        }

    // Use this to prevent a tricky recursive hierarchy
    type AideChangeRequest = 
        { Info : ChangeRequestInfo 
          AssetStructureChanges : AssetStructureChange list 
        }

    type ChangeSchemeInfo = 
        { SchemeId : int64
          Code : string
          Name : string
          SolutionProvider : string
        }

    type ChangeScheme = 
        { Info : ChangeSchemeInfo
          SimpleChanges : AttributeChangeRequest list
          StructureChanges : AideChangeRequest list
        }