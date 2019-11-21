#r "netstandard"
#r "System.Text.Encoding.dll"
open System
open System.IO

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


open FSharp.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"

#I @"C:\Users\stephen\.nuget\packages\system.io.packaging\4.5.0\lib\netstandard1.3"
#r "System.IO.Packaging"
#I @"C:\Users\stephen\.nuget\packages\DocumentFormat.OpenXml\2.9.1\lib\netstandard1.3"
#r "DocumentFormat.OpenXml"

#I @"C:\Users\stephen\.nuget\packages\sheetdoc\1.0.0-alpha-20191121a\lib\netstandard2.0"
#r "SheetDoc.dll"
open SheetDoc.Internal.Render
open SheetDoc.SheetDoc


#load "..\src\AssetPatch\Base\Addendum.fs"
#load "..\src\AssetPatch\Base\Common.fs"
#load "..\src\AssetPatch\Base\AssocList.fs"
#load "..\src\AssetPatch\Base\CompilerMonad.fs"
#load "..\src\AssetPatch\Base\ChangeFile.fs"
#load "..\src\AssetPatch\Base\Acronyms.fs"
#load "..\src\AssetPatch\Base\AbsChangeFile.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\src\AssetPatch\Uxl\UxlChangeFile.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchTypes.fs"
#load "..\src\AssetPatch\TemplatePatcher\Hierarchy.fs"
#load "..\src\AssetPatch\TemplatePatcher\Template.fs"
#load "..\src\AssetPatch\TemplatePatcher\Renamer.fs"
#load "..\src\AssetPatch\TemplatePatcher\Emitter.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchGen.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchCompiler.fs"
#load "..\src\AssetPatch\TemplatePatcher\Catalogue.fs"
open AssetPatch.Base.Common
open AssetPatch.Base.CompilerMonad
open AssetPatch.Base.FuncLocPath
open AssetPatch.Uxl.UxlChangeFile
open AssetPatch.TemplatePatcher.PatchTypes
open AssetPatch.TemplatePatcher.Template
open AssetPatch.TemplatePatcher.PatchCompiler
open AssetPatch.TemplatePatcher.Catalogue

let outputDirectory () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output")

let outputFile (relFileName : string) : string = 
    Path.Combine(outputDirectory (), relFileName)


let assetConditionTemplate : Class1<uint32> = fun year ->
    asset_condition 
        [ condition_grade Good
          condition_grade_reason "NEW"
          performance_grade Availability_95 
          performance_grade_reason "NEW"
          loading_factor Satisfactory
          loading_factor_reason "NEW"
          survey_date year
        ]

   
let test01 () : Result<unit, ErrMsg> = 
    let worklist : (EquipmentCode * uint32) list= 
        [ ("101001407", 2019u)
        ; ("101001409", 2019u)
        ]   
        |> List.map (fun (a,b) -> (EquipmentCode a, b))
    runCompiler (defaultEnv "TETLEYS")
        <| compileClassEquiValuaEquiPatches 
                    (outputDirectory ())
                    "asset_condition"
                    assetConditionTemplate
                    worklist



let aibReferenceTemplate : Class1<string> = fun sai ->
    aib_reference 
        [ ai2_aib_reference sai
          s4_aib_reference ()
        ]

let test02 () = 
    let worklist = 
        [ ("KRI03-EDC", "SAI00970234")
        ] 
        |> List.map (fun (name, v) -> (FuncLocPath.Create name, v))
    runCompiler (defaultEnv "TETLEYS")
       <| compileClassFlocValuaFlocPatches 
                   (outputDirectory ())
                   "aib_reference"
                   aibReferenceTemplate
                   worklist


let excel01 () : unit = 
    let crDetails = 
        { Description = "CAS14 SWS Copy"
          TypeOfChangeRequest = "AIWEAM0P"
          DraftMode = false
          FlFunctionLocation = [ "BRI02-SSS"; "BRI02-SSS-SWS"; "BRI02-SSS-POW"]
          EqEquipment = []
          ProcessRequester = "ASSET DATA"
        }

    let funcLocs = 
        [ { FunctionalLocation = "BRI02-SSS"
            MaskedFuncLoc = "BRI02-SSS"
            Description = "Site Support Service"
            FuncLocCat = 2u
            StrIndicator = "YW-GS"
            ObjectType = "SSS"
            StartUpDate = new DateTime(year=2019, month=11, day=20)
            SupFunctLoc = "BRI02"
           }
        ; { FunctionalLocation = "BRI02-SSS-SWS"
            MaskedFuncLoc = "BRI02-SSS-SWS"
            Description = "Site Water Services"
            FuncLocCat = 3u
            StrIndicator = "YW-GS"
            ObjectType = "SWS"
            StartUpDate = new DateTime(year=2019, month=11, day=20)
            SupFunctLoc = "BRI02-SSS"
           }
        ]
    let doc1 = 
        spreadsheet 
            [ mmopChangeRequestDetails crDetails
            ; mmopFunctionalLocationData funcLocs
            ]
    renderSpreadSheetDoc doc1 (outputFile "test01.xlsx")