﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module Datatypes =
    
    
    
    
    type StructureItem = 
        { Name: string 
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
 
    type Hierarchy = 
        val private StructureItems : StructureItem list

        new (items : StructureItem list) = 
            let fn (s1 : StructureItem) = s1.CommonName
            { StructureItems = List.sortBy fn items }

        member x.Size 
            with get () : int = x.StructureItems.Length


        member x.Items 
            with get () : StructureItem list = x.StructureItems

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

        
    /// This represents a change on an attribute.
    /// The report on the system that we use for input only 
    /// shows changes. If an attribute is unchanged there will 
    /// be no record.
    type AttributeChange = 
        { AssetName : string
          Reference : string
          AttributeName : string
          AiValue : AttributeValue
          AideValue : AttributeValue
        }
        /// Note this should always be true as we only extract changed 
        /// records from the master database
        member x.HasChanged with get () : bool = x.AideValue <> x.AiValue

    type RepeatedAttributeChange = 
        { AssetName : string
          Reference : string
          RepeatedAttributeName : string
          AttributeSetName : string
          AiValue : AttributeValue
          AideValue : AttributeValue
        } 
        /// Note this should always be true as we only extract changed 
        /// records from the master database
        member x.HasChanged with get () : bool = x.AideValue <> x.AiValue

    /// Assets have "properties" { name, common name, manufacturer, 
    /// grid ref... } that are stored inline with the "Asset record". 
    /// Because they are stored inline, "properties" are diffferent
    /// to attributes that are stored in a separate table with a foreign
    /// key back to the asset.
    /// The input report lists both AI and AIDE property values regardless
    /// of whether the value has changed in AIDE.
    type AssetProperty = 
        { PropertyName : string
          AiValue : string
          AideValue : string
        }
        member x.HasChanged with get () : bool = x.AideValue <> x.AiValue

    type AssetChange = 
        { Reference : string
          AiAssetName : string
          AiCommonName : string
          AssetProperties : AssetProperty list
        }
        member x.HasChangedProperties 
            with get () : bool = 
                x.AssetProperties |> List.exists (fun prop -> prop.HasChanged)


    type ChangeRequestInfo = 
        { ChangeRequestId : int64
          RequestType : string
          Status : string
          Comment: string
          RequestTime : System.DateTime
        }


    type ChangeRequest = 
        { Info : ChangeRequestInfo
          AssetChanges : AssetChange list
          AttributeChanges : AttributeChange list
          RepeatedAttributeChanges : RepeatedAttributeChange list
        }


    type ChangeSchemeInfo = 
        { SchemeId : int64
          Code : string
          Name : string
          SolutionProvider : string
        }

    type ChangeScheme = 
        { Info : ChangeSchemeInfo
          ChangeRequests : ChangeRequest list
        }