#r "netstandard"
#r "System.Text.Encoding.dll"
#r "System.Xml.Linq"
#r "System.Xml.ReaderWriter"
#r "System.Xml.XDocument"
#r "System.IO.FileSystem.Primitives"
open System.IO
open System

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
#load "..\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\TemplatePatcher\CommonTypes.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchTypes.fs"
#load "..\src\AssetPatch\TemplatePatcher\Hierarchy.fs"
#load "..\src\AssetPatch\TemplatePatcher\Template.fs"
#load "..\src\AssetPatch\TemplatePatcher\CompilerMonad.fs"
#load "..\src\AssetPatch\TemplatePatcher\EquiIndexing.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchWriter.fs"
#load "..\src\AssetPatch\TemplatePatcher\EmitEquipment.fs"
#load "..\src\AssetPatch\TemplatePatcher\EmitFuncLoc.fs"
#load "..\src\AssetPatch\TemplatePatcher\Emitter.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchCompiler.fs"
#load "..\src\AssetPatch\TemplateCatalogue\Base.fs"
#load "..\src\AssetPatch\TemplateCatalogue\AssetCondition.fs"
#load "..\src\AssetPatch\TemplateCatalogue\Lstnut.fs"
#load "..\src\AssetPatch\TemplateCatalogue\Smonsy.fs"
#load "EdcPatcher\OSGB36.fs"
#load "EdcPatcher\InputData.fs"
#load "EdcPatcher\EdcTemplate.fs"
#load "EdcPatcher\EdcPatcher.fs"
open EdcPatcher.EdcPatcher

let outputDirectory (child : string) : string = 
    match child with 
    | null | "" -> Path.Combine(__SOURCE_DIRECTORY__, @"..\output")
    | _ -> Path.Combine(__SOURCE_DIRECTORY__, @"..\output", child)


let options : EdcOptions = 
    {   UserName = "TETLEYS"
        OutputDirectory = outputDirectory "edc_patcher"
        WorkListPath = @"G:\work\Projects\assets\asset_patch\EnvDischarge_Worklist1.xlsx" 
        UseFlocTemplateIds = false
    }

let main01 () = 
    runEdcPatcherPhase1 options 

let main02 (dirname : string) = 
    runEdcPatcherPhase2 options (outputDirectory dirname)

    
let main03 (dirname : string) = 
    runEdcPatcherPhase3 options (outputDirectory dirname)




