﻿#r "netstandard"
#r "System.Text.Encoding.dll"
open System.Text.RegularExpressions
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
#load "..\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\Base\EntityTypes.fs"
#load "..\src\AssetPatch\PatchBuilder\Common.fs"
#load "..\src\AssetPatch\PatchBuilder\Hierarchy.fs"
#load "..\src\AssetPatch\PatchBuilder\Catalogue.fs"
#load "..\src\AssetPatch\PatchBuilder\Emitter.fs"
#load "..\src\AssetPatch\PatchBuilder\ClassAddPatcher.fs"
open AssetPatch.Base.ChangeFile
open AssetPatch.Base.FuncLocPath
open AssetPatch.Base.EntityTypes
open AssetPatch.PatchBuilder.Hierarchy
open AssetPatch.PatchBuilder.Catalogue
open AssetPatch.PatchBuilder.ClassAddPatcher

let outputFile (relFileName : string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output", relFileName)

let assetConditionTemplate (year : uint32) : Class = 
    asset_condition 
        [ condition_grade Good
          condition_grade_reason "NEW"
          performance_grade Availability_95 
          performance_grade_reason "NEW"
          loading_factor Satisfactory
          loading_factor_reason "NEW"
          survey_date year
        ]



        
let test01 () = 
    let classEquiFile = outputFile "asset_condition_01_classequi.txt"
    let valuaEquiFile = outputFile "asset_condition_02_valuaequi.txt"
    let src : (EquipmentCode * uint32) list= 
        [ ("101001407", 2019u)
        ; ("101001409", 2019u)
        ]   
        |> List.map (fun (a,b) -> (EquipmentCode a, b))
    makeAddEquiPatches classEquiFile valuaEquiFile "TETLEYS" src assetConditionTemplate



let aib_reference_template (sai : string) : Class = 
    aib_reference 
        [ ai2_aib_reference sai
          s4_aib_reference ()
        ]

let test02 () = 
    let classFlocFile = outputFile "aib_reference_01_classfloc.txt"
    let valuaFlocFile = outputFile "aib_reference_02_valuafloc.txt"
    let src = 
        [ ("KRI03-EDC", "SAI00970234")
        ] 
        |> List.map (fun (name, v) -> (FuncLocPath.Create name, v))
    makeAddFlocPatches classFlocFile valuaFlocFile "TETLEYS" src aib_reference_template

