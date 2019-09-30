// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module Attributes =
    
    /// Assets have special attributes (Installed from date, 
    /// Location Ref) that we call Properties
    type Properties = Map<string,string>

    type Attributes = Map<string,string>

    
    type NameValueDiff =
        | OnlyLeft of name : string * value : string
        | Difference of name : string * leftValue : string * rightValue : string
        | OnlyRight of name : string * value : string

    let differenceAttributes (leftAttributes : Attributes) 
                             (rightAttributes : Attributes) : NameValueDiff list = 
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
            
                             

    