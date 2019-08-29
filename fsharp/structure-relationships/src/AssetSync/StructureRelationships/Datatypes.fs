// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.StructureRelationships


module Datatypes =

    open System

    
    type StructureItem = 
        { Name: string 
          CommonName : string 
          Reference : string
        }

 
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