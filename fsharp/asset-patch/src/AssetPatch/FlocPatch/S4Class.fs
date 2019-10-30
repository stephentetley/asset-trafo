// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocPatch



module S4Class =
    
    open System.Text
    
    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.EntityTypes
    

    /// CLASSTYPE is 002 for Equi or 003 for Floc so don't store 
    /// this in the object.
    type S4Class = 
        { ClassName : string
          ClInt : uint32
          AllowedCharacteristics : string list
        }

    let s4ClassToAssocs (entityType: EntityType) (s4Class : S4Class) : AssocList<string, string> =         
        let classtype = 
            match entityType with
            | FuncLoc | ClassFloc | ValuaFloc -> "003"
            | Equi | ClassEqui | ValuaEqui -> "002"
        [ ("CLASS",     s4Class.ClassName)
        ; ("CLASSTYPE", classtype)
        ; ("CLINT",     IntegerString.Create(10, s4Class.ClInt).Number)
        ] |> AssocList.ofList 

    

    let makeClassFloc (funcLoc : FuncLocPath)  (s4Class : S4Class) : ClassFloc = 
        { FuncLoc = funcLoc
          Class = s4Class.ClassName
          ClassType = IntegerString.OfString "003"
          ClassNumber = IntegerString.Create(10, s4Class.ClInt)
          Status = 1
          }

    let makeClassEqui (equiNumber : IntegerString)  (s4Class : S4Class) : ClassEqui = 
        { EquipmentNumber = equiNumber
          Class = s4Class.ClassName
          ClassType = IntegerString.OfString "002"
          ClassNumber = IntegerString.Create(10, s4Class.ClInt)
          Status = 1
          }


    let makeClassAssocs (s4Class : S4Class) (funcLocs : string list) : AssocList<string, string> list = 
        let make1 funcLoc = 
           s4ClassToAssocs ClassFloc s4Class 
                |> AssocList.cons "FUNCLOC" funcLoc
                |> fun xs -> AssocList.snoc xs "CLSTATUS1" "1"
        List.map make1 funcLocs

    let makeAllAssocs (flocClasses : S4Class list) (funcLocs : string list) : AssocList<string, string> list = 
        List.map (fun x -> makeClassAssocs x funcLocs) flocClasses 
            |> List.concat

    // These might be better in Sqlite or a csv file
    let clAIB_REFERENCE : S4Class =
        { ClassName = "AIB_REFERENCE"
          ClInt = 850u 
          AllowedCharacteristics = [ "AI2_AIB_REFERENCE"; "S4_AIB_REFERENCE" ] }
    
    let clEAST_NORTH : S4Class =
        { ClassName = "EAST_NORTH"
          ClInt = 379u 
          AllowedCharacteristics = [ "EASTING"; "NORTHING" ]}

    let clUNICLASS_CODE : S4Class =
        { ClassName = "UNICLASS_CODE"
          ClInt = 905u 
          AllowedCharacteristics = [ "UNICLASS_CODE" ] }

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

   
    let clLSTNUT : S4Class =
        { ClassName = "LSTNUT"
          ClInt = 973u 
          AllowedCharacteristics  = 
              [ "LSTN_RELAY_1_FUNCTION"
              ; "..."
              ]
        }