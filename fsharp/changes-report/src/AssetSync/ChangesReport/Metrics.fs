﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module Metrics =
    
    
    open AssetSync.ChangesReport.Datatypes



    let numberOfChangeRequests (scheme : ChangeScheme) : int = 
        scheme.ChangeRequests.Length

    let crNumberOfPropertyChanges (scheme : ChangeRequest) : int = 
        let step (change : AssetChange) = 
            change.AssetProperties |> List.filter (fun x -> x.HasChanged) |> List.length
        List.sumBy step scheme.AssetChanges

    let crNumberOfAttributeChanges (scheme : ChangeRequest) : int = 
        let step (change : AttributeChange) = 
            if change.HasChanged then 1 else 0
        List.sumBy step scheme.AttributeChanges

    let crNumberOfRepeatedAttributeChanges (scheme : ChangeRequest) : int = 
        let step (change : RepeatedAttributeChange) = 
            if change.HasChanged then 1 else 0
        List.sumBy step scheme.RepeatedAttributeChanges


    let numberOfPropertyChanges (scheme : ChangeScheme) : int = 
        scheme.ChangeRequests |> List.sumBy crNumberOfPropertyChanges


    let numberOfAttributeChangesAll (scheme : ChangeScheme) : int = 
        let a = scheme.ChangeRequests |> List.sumBy crNumberOfAttributeChanges
        let b = scheme.ChangeRequests |> List.sumBy crNumberOfRepeatedAttributeChanges
        a + b