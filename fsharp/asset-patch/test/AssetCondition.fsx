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
#load "..\src\AssetPatch\FlocPatch\Common.fs"
open AssetPatch.Base
open AssetPatch.Base.ChangeFile
open AssetPatch.Base.CompilerMonad
open AssetPatch.Base.EntityTypes
open AssetPatch.FlocPatch.Common


let outputFile (relFileName : string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output", relFileName)

// Adding ASSET_CONDITION ...

// PERFORMANCE_GRADE_REASON
// LOADING_FACTOR_REASON
// CONDITION_GRADE_REASON
// CONDITION_GRADE
// PERFORMANCE_GRADE
// SURVEY_DATE
// LOADING_FACTOR

/// CLASSTYPE is 002 for Equi or 003 for Floc so don't store 
/// this in the object.
type S4Class = 
    { ClassName : string
      ClInt : uint32
      AllowedCharacteristics : string list
    }

let makeClassEqui (equiNumber : IntegerString)  (s4Class : S4Class) : ClassEqui = 
    { EquipmentNumber = equiNumber
      Class = s4Class.ClassName
      ClassType = IntegerString.OfString "002"
      ClassNumber = IntegerString.Create(10, s4Class.ClInt)
      Status = 1
      }

let clASSET_CONDITION : S4Class =
    { ClassName = "ASSET_CONDITION"
      ClInt = 1013u 
      AllowedCharacteristics  = 
        [ "PERFORMANCE_GRADE_REASON"
        ; "LOADING_FACTOR_REASON"
        ; "CONDITION_GRADE_REASON"
        ; "CONDITION_GRADE"
        ; "PERFORMANCE_GRADE"
        ; "SURVEY_DATE"
        ; "LOADING_FACTOR" ]
    }
let makeASSET_CONDITION (equiNumber : IntegerString) : ClassEqui = 
    makeClassEqui equiNumber clASSET_CONDITION

let makeASSET_CONDITIONValues (equiNumber : IntegerString) (year : uint32): ValuaEqui list = 
    let make charId value = 
        { EquipmentNumber = equiNumber
          ClassType = IntegerString.OfString "002"
          CharacteristicID = charId
          CharacteristicValue = value
          Attributes = AssocList.empty
        }
    [ make "PERFORMANCE_GRADE_REASON" "NEW"
    ; make "LOADING_FACTOR_REASON" "NEW"
    ; make "CONDITION_GRADE_REASON" "NEW"
    ; make "CONDITION_GRADE" "1 - GOOD"
    ; make "PERFORMANCE_GRADE" "1 - AVAILABILITY 95%"
    ; make "SURVEY_DATE" (year.ToString())
    ; make "LOADING_FACTOR" "3 - SATISFACTORY"
    ]

        
let test01 () = 
    let outPath = outputFile "asset_condition_01_valua.txt"
    let equipmentIds = 
        [ 101001407u; 101001410u ] |> List.map (fun x -> IntegerString.Create(9, x))
    let values = 
        equipmentIds 
            |> List.map (fun x -> makeASSET_CONDITIONValues x 2019u)
            |> List.concat  
    evalCompiler () () <|
        compile {
            let! changeFile = compileValuaEquiFile "TETLEYS" System.DateTime.Now values
            do! writeChangeFileAndMetadata outPath changeFile
            return ()
        }

