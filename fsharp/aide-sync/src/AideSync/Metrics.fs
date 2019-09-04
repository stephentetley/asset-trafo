// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module Metrics =
    
    
    open AideSync.Datatypes

    let numberOfDifferences (diffs : Differences) : int = 
        let count1 (sd : StructureItemDiff) : int = 
            match sd with
            | Match _ -> 0
            | _ -> 1
        List.sumBy count1 diffs

    let numberOfChangeRequests (scheme : ChangeScheme) : int = 
        scheme.ChangeRequests.Length

    let crNumberOfPropertyChanges (request : ChangeRequest) : int = 
        let step (change : AssetChange) = 
            change.AssetChanges.AssetProperties 
                |> List.filter (fun x -> x.HasChanged) |> List.length
        match request with
        | AttributeChange(_,changes) -> List.sumBy step changes
        | _ -> 0

    let crNumberOfAttributeChanges (request : ChangeRequest) : int = 
        let step (change : AssetChange) = 
            change.AssetChanges.AttrChanges
                |> List.filter (fun x -> x.HasChanged) |> List.length
        match request with
        | AttributeChange(_,changes) -> List.sumBy step changes
        | _ -> 0

    let crNumberOfRepeatedAttributeChanges (request : ChangeRequest) : int = 
        let step (change : AssetChange) = 
            change.AssetChanges.RepeatedAttrChanges
                |> List.filter (fun x -> x.HasChanged) |> List.length
        match request with
        | AttributeChange(_,changes) -> List.sumBy step changes
        | _ -> 0

    let numberOfPropertyChanges (scheme : ChangeScheme) : int = 
        scheme.ChangeRequests |> List.sumBy crNumberOfPropertyChanges


    let numberOfAttributeChangesAll (scheme : ChangeScheme) : int = 
        let a = scheme.ChangeRequests |> List.sumBy crNumberOfAttributeChanges
        let b = scheme.ChangeRequests |> List.sumBy crNumberOfRepeatedAttributeChanges
        a + b