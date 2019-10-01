// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.Old


module Metrics =
    
    
    open AideSync.Old.Datatypes

    let internal numberOfDifferences (diffs : Differences) : int = 
        let count1 (sd : StructureItemDiff) : int = 
            match sd with
            | Match _ -> 0
            | _ -> 1
        List.sumBy count1 diffs


    let crNumberOfPropertyChanges (request : AttributeChangeRequest) : int = 
        let step (change : AssetChange) = 
            change.AssetChanges.AssetProperties
                |> List.filter (fun x -> x.HasChanged) |> List.length
        List.sumBy step request.AssetChanges
        

    let crNumberOfAttributeChanges (request : AttributeChangeRequest) : int = 
        let step (change : AssetChange) = 
            change.AssetChanges.AttrChanges
                |> List.filter (fun x -> x.HasChanged) |> List.length
        List.sumBy step request.AssetChanges

    let crNumberOfRepeatedAttributeChanges (request : AttributeChangeRequest) : int = 
        let step (change : AssetChange) = 
            change.AssetChanges.RepeatedAttrChanges
                |> List.filter (fun x -> x.HasChanged) |> List.length
        List.sumBy step request.AssetChanges


    
    let numberOfChangeRequests (scheme : ChangeScheme) : int = 
        scheme.SimpleChanges.Length + scheme.StructureChanges.Length


    //let numberOfPropertyChanges (scheme : ChangeScheme) : int = 
    //    scheme.ChangeRequests |> List.sumBy crNumberOfPropertyChanges


    //let numberOfAttributeChangesAll (scheme : ChangeScheme) : int = 
    //    let a = scheme.SimpleChanges 
    //                |> List.sumBy (fun x -> crNumberOfAttributeChanges x.AssetChanges)
    //    let b = scheme.StructureChanges |> List.sumBy crNumberOfRepeatedAttributeChanges
    //    a + b