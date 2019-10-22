﻿// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Text.Encoding.dll"
open System.IO

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


open FSharp.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"

#load "..\src\AssetPatch\Base\AssocList.fs"
#load "..\src\AssetPatch\Base\Common.fs"
#load "..\src\AssetPatch\Base\Syntax.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\Base\Markdown.fs"
#load "..\src\AssetPatch\Base\CompilerMonad.fs"
#load "..\src\AssetPatch\FuncLocBuilder\FuncLocPath.fs"
#load "..\src\AssetPatch\FuncLocBuilder\FuncLoc.fs"
#load "..\src\AssetPatch\FuncLocBuilder\FuncLocCommon.fs"
#load "..\src\AssetPatch\FuncLocBuilder\FuncLocPatch.fs"
#load "..\src\AssetPatch\FuncLocBuilder\ClassFlocPatch.fs"
#load "..\src\AssetPatch\FuncLocBuilder\FuncLocMonad.fs"
open AssetPatch.Base
open AssetPatch.Base.Syntax
open AssetPatch.Base.Parser
open AssetPatch.Base.Printer
open AssetPatch.Base.Markdown
open AssetPatch.FuncLocBuilder
open AssetPatch.FuncLocBuilder.FuncLocPatch
open AssetPatch.FuncLocBuilder.ClassFlocPatch
open AssetPatch.FuncLocBuilder.FuncLocMonad


let outputFile (relFileName : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output", relFileName)

let demo01 () = 
    let header = HeaderRow [| "FUNCLOC"; "TXTMI"; "ABCKZFLOC" |]
    let patch = 
        { PatchType = Download 
          DataModel = U1
          EntityType = FuncLoc
          Variant = ()
          User = "FORDB"
          DateTime = System.DateTime.Now
          Selection = 
            [   FuncLocEq "BIR23-EDC" 
                FuncLocEq "BIR23-EDC-LQD" 
                FuncLocEq "BIR23-EDC-LQD-RGM" 
                FuncLocEq "BIR23-EDC-LQD-RGM-SYS01" 
            ]
          HeaderRow = header
          DataRows = 
            [   DataRow [| "BIR23-EDC" ; "Environmental Discharge"; "" |]
                DataRow [| "BIR23-EDC-LQD" ; "Liquid Discharge"; "" |]
                DataRow [| "BIR23-EDC-LQD-RGM" ; "Regulatory Monitoring"; "" |]
                DataRow [| "BIR23-EDC-LQD-RGM-SYS01" ; "EA Event duration Monitoring"; "" |]
            ]
        }
    patchToString patch |> printfn "%s"


let demo02 () = 
    FParsec.CharParsers.run pPatchType "* Download" 

let demo03 () = 
    let source = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm\Functional_Location.txt"
    let outpath = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm\fl_out.txt"
    match readPatch source with
    | Result.Error msg -> Result.Error msg
    | Result.Ok ans ->
        writePatch outpath ans
        // List.iter (printfn "%O") ans.RowAssocs
        Result.Ok ans.RowAssocs


let showHeaders (filePath : string) = 
    match readPatch filePath with
    | Result.Error msg -> failwith msg
    | Result.Ok ans ->
        List.iter (printfn "%s") ans.ColumnHeaders

let demo04 () =
    let source = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm\Equipment.txt"
    showHeaders source        


let demo05 () = 
    let source = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm\Functional_Location.txt"
    match readPatch source with
    | Result.Error msg -> failwith msg
    | Result.Ok ans ->
        let ix = ans.ColumnIndex "ANLNRI"
        ans.DataRows |> List.map (fun row -> row.GetItem(ix))


let summarize (inputPatch : string) : Result<unit, string> = 
    let filename = 
        Path.GetFileName(inputPatch) 
            |> fun s -> Path.ChangeExtension(path = s, extension = "html")

    let outputHtml = outputFile filename
    let pathToCss = @"..\..\..\..\..\libs\markdown-css-master\github.css"
    match readPatch inputPatch with
    | Result.Error msg -> Result.Error msg
    | Result.Ok ans ->
        let opts = pandocHtmlDefaultOptions pathToCss
        pandocGenHtml opts outputHtml ans

let demo06 () = 
    let sources = 
        System.IO.Directory.GetFiles( path = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm"
                                    , searchPattern = "*.txt")
    Array.iter (summarize >> ignore) sources

let demo07 () = 
    FuncLocPath.Create "BIR23-EDC" 

let demo08 () = 
    let source = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm\Functional_Location.txt"
    match FuncLoc.getRootFromPathFile "BIR23-EDC" source with
    | Result.Error msg -> Result.Error msg
    | Result.Ok root -> 
        let f1 = FuncLoc.extend "LQD" "Liquid Discharge" "LQD" root
        runFLCompiler <| makeFuncLocPatch "FORDB" System.DateTime.Now [f1]

let demo08a () = 
    AssocList.ofList [ ("FUNCLOC", "BIR23-EDC")]
        |> DataRow.FromAssocList


let demo09 () = 
    let source = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm\Functional_Location.txt"
    let outfile = outputFile "test_patch_funcloc01.txt"
    let action = 
        flocBuilder {
            let! r1 =  root "BIR23-EDC" 
            let! smon = 
                r1  |> extend "EDG" "Environmental Discharge" "EDC"
                    >>= extend "LQD" "Liquid Discharge" "LQD"
                    >>= extend "SYS01" "EA Monitoring System" "SMON"                
            return ()
        }
    compilePatch { PathToFlocFile = source; User = "FORDB"; Timestamp = System.DateTime.Now }
            action
            outfile

