// Copyright (c) Stephen Tetley 2019

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
#load "..\src\AssetPatch\Base\AbsPatch.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\Base\Markdown.fs"
#load "..\src\AssetPatch\Base\CompilerMonad.fs"
#load "..\src\AssetPatch\Base\QueryPatch.fs"
#load "..\src\AssetPatch\Base\TidyPatch.fs"
#load "..\src\AssetPatch\FlocPatch\Common.fs"
#load "..\src\AssetPatch\FlocPatch\FuncLocPath.fs"
#load "..\src\AssetPatch\FlocPatch\FuncLoc.fs"
#load "..\src\AssetPatch\FlocPatch\FuncLocPatch.fs"
#load "..\src\AssetPatch\FlocPatch\ClassFlocPatch.fs"
#load "..\src\AssetPatch\FlocPatch\FlocPatchMonad.fs"
open AssetPatch.Base
open AssetPatch.Base.Syntax
open AssetPatch.Base.Parser
open AssetPatch.Base.Printer
open AssetPatch.Base.Markdown
open AssetPatch.Base.TidyPatch
open AssetPatch.FlocPatch.FlocPatchMonad


let outputDirectory () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output")

let outputFile (relFileName : string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output", relFileName)



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

// let eaMonitoringPatch (rootName : string) = 


let compilePatch01 () = 
    let source = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm\aco01_funcloc_file_download.txt"
    let action = 
        flocpatch {
            let! path =  
                root "ACO01" 
                    >>= extend "EDG" "Environmental Discharge" "EDC"
                    >>= extend "LQD" "Liquid Discharge" "LQD"
                    >>= extend "SYS01" "EA Monitoring System" "SMON"                
            return ()
        }
    compilePatch { PathToFlocFile = source; User = "TETLEYS"; Timestamp = System.DateTime.Now }
            action
            "ACO01"
            (outputDirectory ())

let tidy01 () = 
    let src = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm\ACO01_funcloc_file_download.txt"
    let dest = outputFile "ACO01_funcloc_tidy.txt"
    tidyPatch ["FUNCLOC"; "TXTMI"] [] src dest



