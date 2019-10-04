// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.AideReport


module Attributes =
    
    /// Assets have special attributes (Installed from date, 
    /// Location Ref) that we call Properties
    type Properties = Map<string,string>

    type Attributes = Map<string,string>

    
    type NameValueDiff =
        | OnlyLeft of name : string * value : string
        | Difference of name : string * leftValue : string * rightValue : string
        | OnlyRight of name : string * value : string

    type AttributeDiffs = NameValueDiff list
    type PropertyDiffs = NameValueDiff list

    type NodeChanges = 
        { PropertyChanges : PropertyDiffs
          AttributeChanges : AttributeDiffs 
          RepeatedAttributeChanges : AttributeDiffs }

        member x.HasChanges 
            with get () : bool = 
                not (x.PropertyChanges.IsEmpty) ||
                not (x.AttributeChanges.IsEmpty) ||
                not (x.RepeatedAttributeChanges.IsEmpty)

    let addDifference (name : string) 
                      (valueLeft : string option) 
                      (valueRight : string option) 
                      (diffList : AttributeDiffs) : AttributeDiffs = 
        match valueLeft, valueRight with
        | Some a, Some b -> 
            if a <> b then Difference(name, a, b) :: diffList else diffList
        | Some a, None -> OnlyLeft(name, a) :: diffList
        | None, Some b -> OnlyRight(name, b) :: diffList
        | _, _ -> diffList


    /// Maybe not needed...
    let differenceAttributes (leftAttributes : Attributes) 
                             (rightAttributes : Attributes) : AttributeDiffs = 
        let rec work lefts rights cont = 
            match lefts,rights with
            | xs, [] -> cont (List.map (fun (n,v) -> OnlyLeft(n,v)) xs)
            | [], ys -> cont (List.map (fun (n,v) -> OnlyRight(n,v)) ys)
            | ((nameLeft,valueLeft)::xs, (nameRight,valueRight)::ys) ->                 
                match compare nameLeft nameRight with
                | i when i = 0 -> 
                    work xs ys (fun ac -> 
                    if valueLeft = valueRight then
                        cont ac
                    else cont (Difference(nameLeft, valueLeft, valueRight) :: ac))
                    
                | i when i < 0 -> 
                    // x is not in (y::ys)                 
                    work xs rights (fun ac -> 
                    cont (OnlyLeft(nameLeft,valueLeft) :: ac))
                | i when i > 0 -> 
                    // y is not in (x::xs)
                    work lefts ys (fun ac -> 
                    cont (OnlyRight(nameRight,valueRight) :: ac))
                | i -> failwithf "differenceL - Weird (impossible) pattern failure: %i" i
        work (Map.toList leftAttributes) (Map.toList rightAttributes) (fun x -> x)
            
                             
    /// Value change might be a change to an attribute 
    /// (possibly repeated) or a so called 'property' like manufacturer.
    type ValueChange = 
        { FieldName : string
          LeftValue : string
          RightValue : string
          Description : string
          }


    let valueChange (description : string) 
                             (valueDiff : NameValueDiff) : ValueChange = 
        match valueDiff with
        | OnlyLeft (name,left) ->
            { FieldName = name; LeftValue = left; 
              RightValue = ""; Description = description }

        | Difference (name,left,right) ->
            { FieldName = name; LeftValue = left; 
              RightValue = right; Description = description }

        | OnlyRight (name,right) ->
            { FieldName = name; LeftValue = ""; 
              RightValue = right; Description = description }


    let attributeValueChanges (changes : NodeChanges) : ValueChange list = 

        [ List.map (valueChange "") changes.AttributeChanges
        ; List.map (valueChange "Repeated") changes.RepeatedAttributeChanges
        ] |> List.concat |> List.sortBy (fun x -> x.FieldName)


    let propertyValueChanges (changes : NodeChanges) : ValueChange list = 
        changes.PropertyChanges
            |> List.map (valueChange "")
            |> List.sortBy (fun x -> x.FieldName)

            