// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO



#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"
open SLFormat.CommandOptions


#I @"C:\Users\stephen\.nuget\packages\slpotassco\1.0.0-alpha-20190723a\lib\netstandard2.0"
#r "SLPotassco"
open SLPotassco.Potassco.Base
open SLPotassco.Potassco.Invoke


let clingoDirectory () = 
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, @"..\clingo")


// For sl-format

let doubleDoubleQuote (s:string) : string = "\"\"" + s + "\"\""
let tripleDoubleQuote (s:string) : string = "\"\"\"" + s + "\"\"\""
let singleDoubleQuote (s:string) : string = "'\"" + s + "\"'"
let doubleSingleQuote (s:string) : string = "\"'" + s + "'\""

// For sl-potassco
let clingoFailureDescription (clingoFailure : RunClingoFailure) : string = 
    match clingoFailure with
    | SysFail exc -> sprintf "*** SYSTEM: %s" exc.Message
    | ClingoFail x -> sprintf "%s\n%s" x.Error x.Info
    | OutputParseFail msg -> sprintf "*** Parsing failure: %s" msg


let runQuery (s1 : string) (s2 : string) (s3 : string) : Result<ClingoOutput, string> =  
    let workingDir = clingoDirectory ()
    let setConsts = 
        [ argument "-c" ^+^ literal "argv1" &= tripleDoubleQuote s1
        ; argument "-c" ^+^ literal "argv2" &= tripleDoubleQuote s2
        ; argument "-c" ^+^ literal "argv3" &= tripleDoubleQuote s3
        ]
    printfn "Working Directory: %s" workingDir
    let files = 
        [ "facts/code_mapping.lp"
        ; "facts/aib_common_names.lp"
        ; "facts/aib_installation_type.lp"
        ; "facts/floc_mapping_1_2.lp" 
        ; "queries/floc_mapping/floc_mapping_rules.lp" 
        ; "queries/floc_mapping/query1.lp"
        ]
    match runClingo workingDir (Some 0) [ group setConsts ] files with
    | Error err -> Error (clingoFailureDescription err)
    | Ok output -> Ok output

let tryFloc4Fwd (term : AnswerTerm) : (string list) option = 
    match term with
    | AnswerTerm("floc4_fwd", [String s1; String s2; String s3; String s4]) -> 
        Some [s1;s2;s3;s4]
    | _ -> None

let interpResults (output : ClingoOutput) : string list = 
    let descendants1 (clingoAns : ClingoAnswer) : (string list) option = 
        match clingoAns.Status with
        | Satisfiable -> 
            List.tryPick tryFloc4Fwd clingoAns.AnswerTerms 
        | _ -> None
    List.tryPick descendants1 output.Answers |> Option.defaultValue []

let demo01 () = 
    runQuery "ALWOODLEY/NO 2 SPS" "CONTROL SERVICES" "RTS MONITORING"
        |> Result.map interpResults