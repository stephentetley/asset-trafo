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
open SLFormat.CommandOptions.SimpleInvoke

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20190721\lib\netstandard2.0"
#r "MarkdownDoc.dll"
open MarkdownDoc.Markdown
open MarkdownDoc.Pandoc

#I @"C:\Users\stephen\.nuget\packages\slpotassco\1.0.0-alpha-20190723a\lib\netstandard2.0"
#r "SLPotassco"
open SLPotassco.Potassco.Base
open SLPotassco.Potassco.Invoke



let clingoDirectory () = 
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, @"..\..\..\clingo")


let outputDirectory () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\..\output")

let getOutputFile (relFileName : string) = 
    let dir = outputDirectory () in Path.Combine(dir, relFileName)



// For sl-potassco
let clingoFailureDescription (clingoFailure : RunClingoFailure) : string = 
    match clingoFailure with
    | SysFail exc -> sprintf "*** SYSTEM: %s" exc.Message
    | ClingoFail x -> sprintf "%s\n%s" x.Error x.Info
    | OutputParseFail msg -> sprintf "*** Parsing failure: %s" msg


let pandocHtmlOptions () : PandocOptions = 
    let highlightStyle = argument "--highlight-style" &= argValue "tango"
    let selfContained = argument "--self-contained"
    /// Github style is nicer for tables than Tufte
    /// Note - body width has been changed on both stylesheets
    let css = 
        // path relative to the "working directory" that pandoc is invoked from
        argument "--css" &= doubleQuote @"..\..\..\libs\markdown-css-master\github.css"
    { Standalone = true
      InputExtensions = []
      OutputExtensions = []
      OtherOptions = [ css; highlightStyle; selfContained ]  }


let runQuery (assetType : string) : Result<ClingoOutput, string> =  
    let workingDir = clingoDirectory ()
    let setqvar = sprintf "qvar_asset=\"\"\"%s\"\"\"" assetType
    let setConst = argument "-c" &^^ setqvar
    printfn "Working Directory: %s" workingDir
    let files = 
        [ "facts/aib_rule_table_funcloc.lp"
        ; "facts/aib_base_asset_type.lp" 
        ; "facts/aib_rule_table_equipment.lp"
        ; "queries/equipment_below_floc.lp"
        ]
    match runClingo workingDir (Some 0) [setConst] files with
    | Error err -> Error (clingoFailureDescription err)
    | Ok output -> Ok output

type FlocItem = 
    { ItemType : string 
      Name : string
    }

type Descendants = 
    { Item : FlocItem
      FlocItemsBelow : FlocItem list
      EquipmentBelow : string list
    }

// The simplest way of working with Potassco results might be to write
// "matchers" and run them on the answer terms with List.tryPick and 
// List.choose...

let tryGetFlocBelow (term : AnswerTerm) : FlocItem option = 
    match term with
    | AnswerTerm("get_floc_below", [String x; String y]) -> 
        Some { Name = y; ItemType = x}
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
                                |> Option.defaultValue "<type unknown>"
            let identity = List.tryPick tryIdentity clingoAns.AnswerTerms 
                                |> Option.defaultValue "<Unknown>"
            Some { Item = { Name = identity ; ItemType = typename }
                   FlocItemsBelow = 
                        List.choose tryGetFlocBelow clingoAns.AnswerTerms |> List.sort
                   EquipmentBelow = 
                        List.choose tryGetEquipBelow clingoAns.AnswerTerms |> List.sort
                 }
        | _ -> None
    List.choose descendants1 output.Answers 


let makeReport (descendants : Descendants list) : Markdown = 
    let mdFlocs (flocsBelow : FlocItem list) : Markdown = 
        let flocText (floc : FlocItem) : ParaElement = 
            paraText (text floc.Name ^+^ character ':' ^+^ (underscores <| text floc.ItemType))
        h3 (text "Descendant FLOCs") 
            ^!!^ markdown (unorderedList <| List.map flocText flocsBelow)

    let mdEquipment (equipNames : string list) : Markdown = 
        h3 (text "Descendant Equipment") 
            ^!!^ markdown (unorderedList <| List.map (paraText << text) equipNames)

    let mdDescendants (desc : Descendants) : Markdown = 
        h2 (text desc.Item.Name ^+^ character ':' ^+^ (underscores <| text desc.Item.ItemType))
            ^!!^ mdFlocs desc.FlocItemsBelow
            ^!!^ mdEquipment desc.EquipmentBelow

    h1 (text "Elements Below") :: List.map mdDescendants descendants 
        |> concatMarkdown
       
let exportMarkdown (descendants : Descendants list) : Result<int, string> = 
    let doc = makeReport descendants
    let mdPath = getOutputFile "descendants_report.md"
    doc.Save(columnWidth = 140, outputPath = mdPath)
    runPandocHtml5 true 
                    (outputDirectory ()) 
                    "descendants_report.md"
                    "descendants_report.html" 
                    (Some "Descendants Report")
                    (pandocHtmlOptions ()) 
        


let runReport (assetType : string) : Result<int, string> = 
    match runQuery assetType with
    | Error msg -> Error msg
    | Ok clingoAns -> 
        clingoAns |> getDecendants |> exportMarkdown

let demo01 () = 
    runReport "PLC CONTROL"
        
