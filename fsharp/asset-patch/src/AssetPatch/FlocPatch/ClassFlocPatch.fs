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
    
    type FlocClass = 
        { ClassName : string
          ClassType : int
          ClInt : int
        }

    let flocClassToAssocs (flocClass : FlocClass) : AssocList<string, string> =         
        [ ("CLASS", flocClass.ClassName)
        ; ("CLASSTYPE", sprintf "%03i" flocClass.ClassType)
        ; ("CLINT", sprintf "%010i" flocClass.ClInt)
        ] |> AssocList.ofList 

    let clAIB_REFERENCE : FlocClass =
        { ClassName = "AIB_REFERENCE"
          ClassType = 3
          ClInt = 850}
    
    let clEAST_NORTH : FlocClass =
        { ClassName = "EAST_NORTH"
          ClassType = 3
          ClInt = 379 }

    let clUNICLASS_CODE : FlocClass =
        { ClassName = "UNICLASS_CODE"
          ClassType = 3
          ClInt = 905 }

    let makeClassAssocs (flocClass : FlocClass) (funcLocs : string list) : AssocList<string, string> list = 
        let make1 funcLoc = 
            flocClassToAssocs flocClass 
                |> AssocList.cons "FUNCLOC" funcLoc
                |> fun xs -> AssocList.snoc xs "CLSTATUS1" "1"
        List.map make1 funcLocs

    let makeAllAssocs (flocClasses : FlocClass list) (funcLocs : string list) : AssocList<string, string> list = 
        List.map (fun x -> makeClassAssocs x funcLocs) flocClasses 
            |> List.concat


    /// TODO - this is a simplification, some flocs will require 
    /// different classes...
    let sampleFlocClasses = 
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
                    |> makeAllAssocs sampleFlocClasses

            return! makeChangeFile ClassFloc user timestamp rows
        }
