// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Text.Encoding.dll"
open System.IO

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec


#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"


#load "..\src\AssetPatch\Base\Syntax.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\Base\Markdown.fs"
open AssetPatch.Base.Syntax
open AssetPatch.Base.Parser
open AssetPatch.Base.Printer
open AssetPatch.Base.Markdown


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
    run pPatchType "* Download" 

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
        ans.DataRows |> List.map (fun row -> row.[ix])


let summarize (inputPatch : string) 
                (outputHtml : string) : Result<unit, string> = 
    let pathToCss = @"..\..\..\..\..\libs\markdown-css-master\github.css"
    match readPatch inputPatch with
    | Result.Error msg -> Result.Error msg
    | Result.Ok ans ->
        let opts = pandocHtmlDefaultOptions pathToCss
        pandocGenHtml opts outputHtml ans

let demo06 () = 
    let source = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm\Functional_Location.txt"
    let output = outputFile "Functional_Location.html"
    summarize source output

