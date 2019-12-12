// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Text.Encoding.dll"
open System
open System.IO

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec

open FSharp.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"

#load "..\src\AssetPatch\Base\Addendum.fs"
#load "..\src\AssetPatch\Base\Common.fs"
#load "..\src\AssetPatch\Base\AssocList.fs"
#load "..\src\AssetPatch\Base\ChangeFile.fs"
#load "..\src\AssetPatch\Base\Acronyms.fs"
#load "..\src\AssetPatch\Base\AbsChangeFile.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\src\AssetPatch\TemplatePatcher\EquiIndexing2.fs"
open AssetPatch.Base
open AssetPatch.Base.Parser
open AssetPatch.TemplatePatcher.EquiIndexing2

type EquiIndex = 
    { Equi : uint32 
      Txtmi : string
      TplnEilo : string
    }

let extractEquiIndex (row : AssocList<string, string>) : EquiIndex option = 
    match AssocList.tryFind3 "EQUI" "TXTMI" "TPLN_EILO" row with
    | Some (a,b,c) -> 
        try 
            let num = uint32 a 
            Some { Equi  = num;  Txtmi = b; TplnEilo = c }
        with
        | _ -> None
    | None -> None

let demo01 () = 
    readChangeFile @"G:\work\Projects\assets\asset_patch\samples\equi_sample_to_derive_indexing2.txt"
        |> Result.map (fun x -> x.RowAssocs () |> List.map extractEquiIndex |> List.choose id)
        |> Result.map (List.iter (printfn "%O"))

let demo02 () = 
    readEquiDownload @"G:\work\Projects\assets\asset_patch\samples\equi_sample_to_derive_indexing2.txt"
   