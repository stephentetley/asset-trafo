#r "netstandard"
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
#load "..\src\AssetPatch\FlocBuilder\Common.fs"
#load "..\src\AssetPatch\FlocBuilder\Hierarchy.fs"
#load "..\src\AssetPatch\FlocBuilder\Catalogue.fs"
open AssetPatch.Base
open AssetPatch.Base.ChangeFile
open AssetPatch.Base.CompilerMonad
open AssetPatch.Base.EntityTypes
open AssetPatch.FlocBuilder.Common
open AssetPatch.FlocBuilder.Hierarchy
open AssetPatch.FlocBuilder.Catalogue


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

let dummy01 () = 
    assetConditionTemplate 2019u



let makeClassEqui (equiNumber : IntegerString)  (clazz : Class) : ClassEqui = 
    { EquipmentNumber = equiNumber
      Class = clazz.ClassName
      ClassType = IntegerString.OfString "002"
      ClassNumber = IntegerString.Create(10, clazz.ClassInt)
      Status = 1
      }

let makeValuaEqui (equiNumber : IntegerString) (charac : Characteristic) : ValuaEqui = 
    { EquipmentNumber = equiNumber
      ClassType = IntegerString.OfString "002"
      CharacteristicID = charac.Name
      CharacteristicValue = charac.Value
      Attributes = AssocList.empty
    }

let translate (equiNumber : IntegerString)  (clazz : Class) : ClassEqui * ValuaEqui list = 
    let ce = makeClassEqui equiNumber clazz
    (ce, [])

        
let test01 () = 
    let outPath1 = outputFile "asset_condition_01_classequi.txt"
    let outPath2 = outputFile "asset_condition_02_valuaequi.txt"
    let equipmentIds = 
        [ 101001407u; 101001410u ] |> List.map (fun x -> IntegerString.Create(9, x))
    let classes = 
        List.map (fun uid -> makeClassEqui uid (assetConditionTemplate 2019u)) equipmentIds
            

    evalCompiler () () <|
        compile {
            // let! changeFile = compileValuaEquiFile "TETLEYS" System.DateTime.Now values
            let! classChangeFile = compileClassEquiFile "TETLEYS" System.DateTime.Now classes
            do! writeChangeFileAndMetadata outPath1 classChangeFile
            return ()
        }

