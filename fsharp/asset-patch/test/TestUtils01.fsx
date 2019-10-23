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
#load "..\src\AssetPatch\Base\CompilerMonad.fs"
#load "..\src\AssetPatch\Base\QueryPatch.fs"
#load "..\src\AssetPatch\Utilities\PatchReport.fs"
#load "..\src\AssetPatch\Utilities\TidyPatch.fs"

open AssetPatch.Utilities.PatchReport
open AssetPatch.Utilities.TidyPatch


let outputDirectory () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output")

let outputFile (relFileName : string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output", relFileName)



let patchReport01 () = 
    let pathToCss = @"..\..\..\..\..\libs\markdown-css-master\github.css"
    let sources = 
        System.IO.Directory.GetFiles( path = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm"
                                    , searchPattern = "*.txt")
    Array.iter (patchReport pathToCss None >> ignore) sources


let tidyPatch01 () = 
    let src = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm\ACO01_funcloc_file_download.txt"
    let dest = outputFile "ACO01_funcloc_tidy.txt"
    tidyPatch ["FUNCLOC"; "TXTMI"] [] src dest



