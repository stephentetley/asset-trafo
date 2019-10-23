// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Text.Encoding.dll"
open System.IO

#load "..\src\AssetPatch\Base\AssocList.fs"
open AssetPatch.Base


let tryFind01 () : string option = 
    AssocList.tryFind "FUNCLOC" AssocList.empty

let ofList01 () : AssocList<string,string> = 
    AssocList.ofList [("A", "one") ; ("B", "too") ; ("C", "three")]
        |> AssocList.update "B" "two"

let cons01 () : AssocList<string,int> = 
    AssocList.Cons("A", 12, AssocList.empty)

let toList01 () : (string * int) list = 
    AssocList.ofList [("A", 65) ; ("B", 66) ; ("C", 67)]
        |> AssocList.toList

let toListBy01 () : string list = 
    AssocList.ofList [("A", 65) ; ("B", 66) ; ("C", 67)]
        |> AssocList.toListBy (fun x i -> sprintf "%s:%i" x i)


let prioritize01 () : AssocList<string, int> = 
    AssocList.ofList [("C", 67); ("D", 68) ; ("A", 65) ; ("B", 66)]
        |> AssocList.prioritize ["A"; "B"]


// Typing experiments

type AnyLevel = interface end


type IParent<'T> = interface end

type SiteLevel = interface end
    

type FunctionLevel = 
    inherit SiteLevel

    
type ProcessGroupLevel = 
    inherit FunctionLevel

type ProcessLevel = 
    inherit ProcessGroupLevel




type Floc<'level> = Floc of string

let f1 : Floc<ProcessLevel> = Floc "ACO01-EDG-LQD-RGM"

let f2 : Floc<FunctionLevel> = Floc "ACO01-EDG"

//let fail01 () : Bool = 
//    f1 = f2

let ok01 () : Floc<AnyLevel> list = 
    let unwrap (fl : Floc<_>) : Floc<AnyLevel> = match f1 with Floc(x) -> Floc(x)        
    [unwrap f1; unwrap f2]


let typeLevelChild (parent: Floc<'T1>) (child : Floc<'T2>) : bool when 'T2 :> 'T1  = true



