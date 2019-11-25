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
#load "..\src\AssetPatch\Base\CompilerMonad.fs"
#load "..\src\AssetPatch\Base\ChangeFile.fs"
#load "..\src\AssetPatch\Base\Acronyms.fs"
#load "..\src\AssetPatch\Base\AbsChangeFile.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\Utilities\ChangeFileReport.fs"
#load "..\src\AssetPatch\Utilities\TidyChangeFile.fs"
open AssetPatch.Base.Parser
open AssetPatch.Utilities.ChangeFileReport
open AssetPatch.Utilities.TidyChangeFile


let outputDirectory () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output")

let outputFile (relFileName : string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output", relFileName)



let changeFileReport01 () = 
    let pathToCss = @"..\..\..\..\..\libs\markdown-css-master\github.css"
    let sources = 
        System.IO.Directory.GetFiles( path = @"G:\work\Projects\assets\asset_patch\file_download_edm"
                                    , searchPattern = "*.txt")
    Array.iter (changeFileReport pathToCss (outputDirectory ()) >> ignore) sources


let tidyChangeFile01 () = 
    let src = @"G:\work\Projects\assets\asset_patch\file_download_edm\ACO01_funcloc_file_download.txt"
    let dest = outputFile "ACO01_funcloc_tidy.txt"
    tidyChangeFile ["FUNCLOC"; "TXTMI"] [] src dest


let temp01 () = 
    let pathToCss = @"..\..\..\..\..\libs\markdown-css-master\github.css"
    let file = @"G:\work\Projects\assets\asset_patch\file_download_edm\control_automation_04_equi.txt"
    changeFileReport pathToCss (outputDirectory ()) file

let testParser (file : string) = 
    match runParserOnFile (parseChangeFile ()) () file Text.Encoding.UTF8 with
    | FParsec.CharParsers.ParserResult.Failure (str,_,_) -> Result.Error str
    | FParsec.CharParsers.ParserResult.Success (ans,_,_) -> Result.Ok ans

let temp02 () = 
    let file = @"G:\work\Projects\assets\asset_patch\file_download_edm\control_automation_04_equi.txt"
    testParser file

let temp03 () = 
    let file = @"G:\work\Projects\assets\asset_patch\file_download_edm\ACO01_funcloc_file_download.txt"
    testParser file

