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

#load "..\src\AssetPatch\Base\Addendum.fs"
#load "..\src\AssetPatch\Base\AssocList.fs"
#load "..\src\AssetPatch\Base\Common.fs"
#load "..\src\AssetPatch\Base\CompilerMonad.fs"
#load "..\src\AssetPatch\Base\ChangeFile.fs"
#load "..\src\AssetPatch\Base\Acronyms.fs"
#load "..\src\AssetPatch\Base\AbsChangeFile.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\src\AssetPatch\Base\EntityTypes.fs"
#load "..\src\AssetPatch\FlocPatch\Common.fs"
#load "..\src\AssetPatch\FlocPatch\FuncLocPatch.fs"
#load "..\src\AssetPatch\FlocPatch\ClassFlocPatch.fs"
#load "..\src\AssetPatch\FlocPatch\FlocPatchMonad.fs"
open AssetPatch.Base.ChangeFile
open AssetPatch.Base.Parser
open AssetPatch.Base.Printer
open AssetPatch.FlocPatch.FlocPatchMonad


let outputDirectory () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output")

let outputFile (relFileName : string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output", relFileName)




let demo03 () = 
    let source = @"G:\work\Projects\assets\asset_patch\file_download_edm\Functional_Location.txt"
    let outpath = @"G:\work\Projects\assets\asset_patch\file_download_edm\fl_out.txt"
    match readChangeFile source with
    | Result.Error msg -> Result.Error msg
    | Result.Ok ans ->
        writeChangeFile outpath ans
        // List.iter (printfn "%O") ans.RowAssocs
        Result.Ok ans.RowAssocs


let showHeaders (filePath : string) = 
    match readChangeFile filePath with
    | Result.Error msg -> failwith msg
    | Result.Ok ans ->
        List.iter (printfn "%s") ans.ColumnHeaders

let demo04 () =
    let source = @"G:\work\Projects\assets\asset_patch\file_download_edm\Equipment.txt"
    showHeaders source        


let demo05 () = 
    let source = @"G:\work\Projects\assets\asset_patch\file_download_edm\Functional_Location.txt"
    match readChangeFile source with
    | Result.Error msg -> failwith msg
    | Result.Ok ans ->
        let ix = ans.ColumnIndex "ANLNRI"
        ans.DataRows |> List.map (fun row -> row.GetItem(ix))



// let eaMonitoringPatch (rootName : string) = 


let compilePatch01 () = 
    let source = @"G:\work\Projects\assets\asset_patch\file_download_edm\aco01_funcloc_file_download.txt"
    let action = 
        flocpatch {
            let! path =  
                root "ACO01" 
                    >>= extend { Name="EDG"; Description="Environmental Discharge"; ObjectType="EDC" }
                    >>= extend { Name="LQD"; Description="Liquid Discharge";        ObjectType="LQD" }
                    >>= extend { Name="SYS01"; Description="EA Monitoring System";  ObjectType="SMON" }
            return ()
        }
    compilePatch { PathToFlocFile = source; User = "TETLEYS"; Timestamp = System.DateTime.Now }
            action
            "ACO01"
            (outputDirectory ())




