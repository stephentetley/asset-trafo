// Copyright (c) Stephen Tetley 2019

#r "netstandard"
open System
#r "System.Text.Encoding.dll"


#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec

#load "..\src\AssetPatch\Base\Syntax.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
open AssetPatch.Base.Syntax
open AssetPatch.Base.Parser
open AssetPatch.Base.Printer


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
        Result.Ok ans


let demo04 () = 
    let source = @"G:\work\Projects\asset_sync\asset_patch\file_download_edm\Functional_Location.txt"
    match readPatch source with
    | Result.Error msg -> failwith msg
    | Result.Ok ans ->
        List.iter (printfn "%s") ans.ColumnHeaders


