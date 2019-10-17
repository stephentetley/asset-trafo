// Copyright (c) Stephen Tetley 2019

#r "netstandard"

open System

#load "..\src\AssetPatch\Base\Syntax.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
open AssetPatch.Base.Syntax
open AssetPatch.Base.Printer


let demo01 () = 
    let headers = HeaderRow [| "FUNCLOC"; "TXTMI"; "ABCKZFLOC" |]
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
          HeaderRows = [ headers ]
          DataRows = 
            [   DataRow [| "BIR23-EDC" ; "Environmental Discharge"; "" |]
                DataRow [| "BIR23-EDC-LQD" ; "Liquid Discharge"; "" |]
                DataRow [| "BIR23-EDC-LQD-RGM" ; "Regulatory Monitoring"; "" |]
                DataRow [| "BIR23-EDC-LQD-RGM-SYS01" ; "EA Event duration Monitoring"; "" |]
            ]
        }
    printPatch patch |> printfn "%s"


