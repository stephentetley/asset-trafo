// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.0.1\lib\netstandard2.0"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat"
open SLFormat.CommandOptions

#I @"C:\Users\stephen\.nuget\packages\factx\1.0.0-alpha-20190721\lib\netstandard2.0"
#r "FactX"
open FactX
open FactX.FactWriter
open FactX.Skeletons

#I @"C:\Users\stephen\.nuget\packages\slpotassco\1.0.0-alpha-20190723a\lib\netstandard2.0"
#r "SLPotassco"
open SLPotassco.Potassco.Base
open SLPotassco.Potassco.Invoke



let clingoDirectory () = 
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, @"..\..\clingo")

// For sl-potassco
let clingoFailureDescription (clingoFailure : RunClingoFailure) : string = 
    match clingoFailure with
    | SysFail exc -> sprintf "*** SYSTEM: %s" exc.Message
    | ClingoFail x -> sprintf "%s\n%s" x.Error x.Info
    | OutputParseFail msg -> sprintf "*** Parsing failure: %s" msg

let runQuery (assetType : string) : Result<ClingoOutput, string> =  
    let workingDir = clingoDirectory ()
    let setqvar = sprintf "qvar_asset=\"\"\"%s\"\"\"" assetType
    let setConst = argument "-c" &^^ setqvar
    printfn "Working Directory: %s" workingDir
    let files = 
        [ "facts/rule_table_funcloc.lp"
        ; "facts/base_asset_type.lp" 
        ; "facts/rule_table_equipment.lp"
        ; "queries/equipment_below_floc.lp"
        ]
    match runClingo workingDir (Some 0) [setConst] files with
    | Error err -> Error (clingoFailureDescription err)
    | Ok output -> Ok output

type FlocItem = 
    { Name : string
      ItemType : string 
    }

type Descendants = 
    { Item : FlocItem
      FlocItemsBelow : FlocItem list
      EquipmentBelow : string list
    }

//type TermValues = GroundTerm list
//type AnswerMap = Map<string, TermValues list>

//let makeAnswerMap (clingoAns : ClingoAnswer) : AnswerMap option = 
//    let build1 (AnswerTerm(name, vals)) dict = 
//        match Map.tryFind name dict with
//        | Some xs -> Map.add name (vals :: xs) dict
//        | None -> Map.add name [vals] dict
//    match clingoAns.Status with
//    | Satisfiable -> 
//        List.foldBack build1 clingoAns.AnswerTerms Map.empty |> Some
//    | _ -> None


let tryGetString (ix:int) (term:AnswerTerm) : string option = 
    match term with
    | AnswerTerm(_, vals) -> 
        match List.tryItem ix vals with
        | Some (String str)  -> Some str
        | _ -> None



let tryGetInt64 (ix:int) (term:AnswerTerm) : int64 option = 
    match term with
    | AnswerTerm(_, vals) -> 
        match List.tryItem ix vals with
        | Some (Number n)  -> Some n
        | _ -> None

let tryGetSymbolicConstant (ix:int) (term:AnswerTerm) : string option = 
    match term with
    | AnswerTerm(_, vals) -> 
        match List.tryItem ix vals with
        | Some (SymbolicConstant str)  -> Some str
        | _ -> None

//let tryGetFirst (key : string) (dict : AnswerMap) : AnswerTerm option = 
//    match Map.tryFind key dict with
//    | Some (h1 :: _) -> AnswerTerm(key, h1) |> Some
//    | _ -> None

//let getAll (key : string) (dict : AnswerMap) : AnswerTerm list = 
//    match Map.tryFind key dict with
//    | Some xs -> List.map (fun vals -> AnswerTerm(key, vals)) xs
//    | _ -> []



//let getAllTranslate (key : string) 
//                    (translate : AnswerTerm -> 'a) 
//                    (dict : AnswerMap) : 'a list = 
//    match Map.tryFind key dict with
//    | Some xs -> List.map (fun vals -> AnswerTerm(key, vals) |> translate) xs
//    | _ -> []


let tryGetFlocBelow (term : AnswerTerm) : FlocItem option = 
    match term with
    | AnswerTerm("get_floc_below", [String x; String y]) -> Some { Name = y; ItemType = x}
    | _ -> None

let tryGetEquipBelow (term : AnswerTerm) : string option = 
    match term with
    | AnswerTerm("get_equip_below", [String x]) -> Some x
    | _ ->  None 

let tryGetType (term : AnswerTerm) : string option = 
    match term with
    | AnswerTerm("get_type", [String x]) -> Some x
    | _ ->  None 


let tryIdentity (term : AnswerTerm) : string option = 
    match term with
    | AnswerTerm("identity", [String x]) -> Some x
    | _ ->  None 

let getDecendants (output : ClingoOutput) : Descendants list = 
    let descendants1 (clingoAns : ClingoAnswer) : Descendants option = 
        match clingoAns.Status with
        | Satisfiable -> 
            let typename = List.tryPick tryGetType clingoAns.AnswerTerms 
                                |> Option.defaultValue "<Unknown>"
            let identity = List.tryPick tryIdentity clingoAns.AnswerTerms 
                                |> Option.defaultValue "<Unknown>"
            Some { Item = { Name = identity ; ItemType = typename }
                   FlocItemsBelow = List.choose tryGetFlocBelow clingoAns.AnswerTerms 
                   EquipmentBelow = List.choose tryGetEquipBelow clingoAns.AnswerTerms 
                 }
        | _ -> None
    List.choose descendants1 output.Answers 





let runReport (assetType : string) = 
    runQuery assetType 
        |> Result.map getDecendants

