// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocPatch



module ClassFlocPatch =
    
    open System.Text
    
    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.EntityTypes
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.FlocPatch.Common
    

    type private Env = Unit
    type private State = Unit
    type CFCompiler<'a> = CompilerMonad<'a, Env, State>

    let runCFCompiler (action : CFCompiler<'a>) = 
        runCompiler () () action
            |> Result.map fst
    

    /// CLASSTYPE is 002 for Equi or 003 for Floc so don't store 
    /// this in the object.
    type S4Class = 
        { ClassName : string
          ClInt : uint32
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

    let clAIB_REFERENCE : S4Class =
        { ClassName = "AIB_REFERENCE"
          ClInt = 850u }
    
    let clEAST_NORTH : S4Class =
        { ClassName = "EAST_NORTH"
          ClInt = 379u }

    let clUNICLASS_CODE : S4Class =
        { ClassName = "UNICLASS_CODE"
          ClInt = 905u }

    let makeClassAssocs (s4Class : S4Class) (funcLocs : string list) : AssocList<string, string> list = 
        let make1 funcLoc = 
           s4ClassToAssocs ClassFloc s4Class 
                |> AssocList.cons "FUNCLOC" funcLoc
                |> fun xs -> AssocList.snoc xs "CLSTATUS1" "1"
        List.map make1 funcLocs

    let makeAllAssocs (flocClasses : S4Class list) (funcLocs : string list) : AssocList<string, string> list = 
        List.map (fun x -> makeClassAssocs x funcLocs) flocClasses 
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
                            (funcLocs : FuncLoc list) : CFCompiler<ChangeFile> = 
        compile {
            let rows = 
                funcLocs 
                    |> List.sortBy (fun x -> x.Path)
                    |> List.map (fun x -> x.Path.ToString())
                    |> makeAllAssocs sampleS4Classes
            return! makeChangeFile ClassFloc user timestamp rows
        }
