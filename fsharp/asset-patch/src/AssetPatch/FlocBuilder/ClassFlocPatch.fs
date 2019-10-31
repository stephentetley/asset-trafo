// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocBuilder



module ClassFlocPatch =
    
    open System.Text
    
    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.EntityTypes
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.EntityTypes
    open AssetPatch.FlocBuilder.Common
    

    

    /// CLASSTYPE is 002 for Equi or 003 for Floc so don't store 
    /// this in the object.
    type S4Class = 
        { ClassName : string
          ClInt : uint32
        }

    let s4ClassToClassFloc (entityType: EntityType) 
                            (funcLoc : FuncLocPath) 
                            (s4Class : S4Class) : ClassFloc =         
        let classtype = 
            match entityType with
            | FuncLoc | ClassFloc | ValuaFloc -> IntegerString.OfString "003"
            | Equi | ClassEqui | ValuaEqui -> IntegerString.OfString "002"
        { FuncLoc = funcLoc
          Class = s4Class.ClassName
          ClassType = classtype
          ClassNumber = IntegerString.Create(10, s4Class.ClInt)
          Status = 1
        }

    let clAIB_REFERENCE : S4Class =
        { ClassName = "AIB_REFERENCE"
          ClInt = 850u }
    
    let clEAST_NORTH : S4Class =
        { ClassName = "EAST_NORTH"
          ClInt = 379u }

    let clUNICLASS_CODE : S4Class =
        { ClassName = "UNICLASS_CODE"
          ClInt = 905u }

    let makeClassFlocs (funcLoc : FuncLocPath) 
                        (classes : S4Class list) : ClassFloc list = 
        List.map (s4ClassToClassFloc ClassFloc funcLoc) classes

    let makeAllClassFlocs (classes : S4Class list) (funcLocs : FuncLocPath list) : ClassFloc list = 
        List.map (fun x -> makeClassFlocs x classes) funcLocs 
            |> List.concat


    /// TODO - this is a simplification, some flocs will require 
    /// different classes...
    let sampleS4Classes = 
        [ clAIB_REFERENCE 
        ; clEAST_NORTH
        ; clUNICLASS_CODE
        ]


    let makeClassFlocPatch (user : string) 
                            (timestamp : System.DateTime)
                            (funcLocs : FuncLoc list) : CompilerMonad<ChangeFile, 'env, 'acc> = 
        compile {
            let rows =
                funcLocs 
                    |> List.map (fun x -> x.Path)
                    |> makeAllClassFlocs sampleS4Classes
            return! compileClassFlocFile user timestamp rows
        }
