#r "netstandard"
#r "System.Text.Encoding.dll"
#r "System.Xml.Linq"
#r "System.Xml.ReaderWriter"
#r "System.Xml.XDocument"
#r "System.IO.FileSystem.Primitives"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\lib\netstandard2.0"
#r "ExcelProvider.Runtime.dll"

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\typeproviders\fsharp41\netstandard2.0"
#r "ExcelDataReader.DataSet.dll"
#r "ExcelDataReader.dll"
#r "ExcelProvider.DesignTime.dll"

#I @"C:\Users\stephen\.nuget\packages\system.io.packaging\4.5.0\lib\netstandard1.3"
#r "System.IO.Packaging"
#I @"C:\Users\stephen\.nuget\packages\DocumentFormat.OpenXml\2.9.1\lib\netstandard1.3"
#r "DocumentFormat.OpenXml"


#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"

open FSharp.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"

#I @"C:\Users\stephen\.nuget\packages\sheetdoc\1.0.0-alpha-20191121a\lib\netstandard2.0"
#r "SheetDoc.dll"



#load "..\src\AssetPatch\Base\Addendum.fs"
#load "..\src\AssetPatch\Base\Common.fs"
#load "..\src\AssetPatch\Base\AssocList.fs"
#load "..\src\AssetPatch\Base\ChangeFile.fs"
#load "..\src\AssetPatch\Base\Acronyms.fs"
#load "..\src\AssetPatch\Base\AbsChangeFile.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\src\AssetPatch\TemplatePatcher\CommonTypes.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchTypes.fs"
#load "..\src\AssetPatch\TemplatePatcher\TemplateHierarchy.fs"
#load "..\src\AssetPatch\TemplatePatcher\EquiIndexing.fs"
#load "..\src\AssetPatch\TemplatePatcher\Template.fs"
#load "..\src\AssetPatch\TemplatePatcher\CompilerMonad.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchWriter.fs"
#load "..\src\AssetPatch\TemplatePatcher\EmitCommon.fs"
#load "..\src\AssetPatch\TemplatePatcher\EmitEquipment.fs"
#load "..\src\AssetPatch\TemplatePatcher\EmitFuncLoc.fs"
#load "..\src\AssetPatch\TemplatePatcher\Emitter.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchCompiler.fs"
#load "..\src\AssetPatch\TemplateCatalogue\Base.fs"
#load "..\src\AssetPatch\TemplateCatalogue\AssetCondition.fs"
open AssetPatch.Base.Common
open AssetPatch.Base.FuncLocPath
open AssetPatch.TemplatePatcher.Template
open AssetPatch.TemplatePatcher.CompilerMonad
open AssetPatch.TemplatePatcher.PatchCompiler
open AssetPatch.TemplateCatalogue

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

   
//let test01 () : Result<unit, ErrMsg> = 
//    let worklist : (string * uint32) list= 
//        [ ("101001407", 2019u)
//        ; ("101001409", 2019u)
//        ]

//    let opts : CompilerOptions = 
//        { UserName = "TETLEYS"
//          UseInterimFlocIds = false
//        }
//    runCompiler opts
//        <| compileClassEquiValuaEquiPatches 
//                    (outputDirectory ())
//                    5
//                    "asset_condition"
//                    assetConditionTemplate
//                    worklist



let aibReferenceTemplate : Class1<string> = fun sai ->
    aib_reference 
        [ ai2_aib_reference sai
          s4_aib_reference ()
        ]

//let test02 () = 
//    let worklist = 
//        [ ("KRI03-EDC", "SAI00970234")
//        ] 
//        |> List.map (fun (name, v) -> (FuncLocPath.Create name, v))

//    let opts : CompilerOptions = 
//        { UserName = "TETLEYS"
//          UseInterimFlocIds = false
//        }
//    runCompiler opts None
//       <| compileClassFlocValuaFlocPatches 
//                   (outputDirectory ())
//                   5
//                   "aib_reference"
//                   aibReferenceTemplate
//                   worklist

