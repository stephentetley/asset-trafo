// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocPatch



module FuncLocPatch =
    
    
    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.Syntax
    open AssetPatch.Base.Typings
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.FlocPatch.Common


    type private Env = Unit
    type FLCompiler<'a> = CompilerMonad<'a, Env>

    let runFLCompiler (action : FLCompiler<'a>) = 
        runCompiler () action


    let private funcLocAssocList (funcLoc : FuncLoc) : FLCompiler<AssocList<string,string>> = 
        compile {
            let magicFields = [ "JOBJN_FL"; "FLOC_REF" ]
            let! parent = liftOption funcLoc.FuncLocPath.Parent
            return 
                funcLoc.InheritedAttributes
                    |> AssocList.removes magicFields
                    |> AssocList.update "FUNCLOC" (funcLoc.FuncLocPath.ToString())
                    |> AssocList.update "TXTMI" funcLoc.Description
                    |> AssocList.update "FLTYP" (funcLoc.Level.ToString())
                    |> AssocList.update "IEQUI" (if funcLoc.Level >= 5 then "X" else "")
                    |> AssocList.update "EQART" funcLoc.ObjectType
                    |> AssocList.update "TPLMA1" (parent.ToString())
                    |> AssocList.update "TPLMA" (parent.ToString())
        }



    let makeFuncLocPatch (user : string) 
                        (timestamp : System.DateTime)
                        (funcLocs : FuncLoc list) : FLCompiler<FuncLocPatch> = 
        compile {
            let! rows = 
                funcLocs |> List.sort |> mapM  funcLocAssocList 
            return! makePatch FuncLoc user timestamp rows
        }

