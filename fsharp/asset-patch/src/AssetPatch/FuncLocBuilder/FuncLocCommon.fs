// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FuncLocBuilder



module FuncLocCommon =
    
    open System.IO

    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.Syntax
    open AssetPatch.Base.CompilerMonad


    let filenameFuncLocs (outputDirectory : string) (root : string) : string = 
        sprintf "%s_01_add_funclocs.txt" (safeName root)
            |> fun x -> Path.Combine(outputDirectory, x)

    let filenameClassFlocs (outputDirectory : string) (root : string) : string = 
        sprintf "%s_02_add_classflocs.txt" (safeName root)
            |> fun x -> Path.Combine(outputDirectory, x)

    let filenameValuaFlocs (outputDirectory : string) (root : string) : string = 
        sprintf "%s_02_add_valuaflocs.txt" (safeName root)
            |> fun x -> Path.Combine(outputDirectory, x)

    /// At least one row exists 
    let getHeaderRow (rows : AssocList<string, string> list) : CompilerMonad<HeaderRow, 'env> = 
        match rows with
        | [] -> throwError "getHeaderRow - empty list"
        | row1 :: _ -> row1 |> AssocList.keys |> HeaderRow |> mreturn


    /// At least one row exists and it must have field ``FUNCLOC``
    let getSelectionIds (rows : AssocList<string, string> list) : CompilerMonad<SelectionId list, 'env> = 
        let build1 assocs = 
            match AssocList.tryFind "FUNCLOC" assocs with
            | None -> None
            | Some funcloc -> FuncLocEq funcloc |> Some        
        mapM (liftOption << build1) rows |>> List.distinct


    let makePatch (entityType : EntityType) 
                    (user : string) 
                    (timestamp : System.DateTime)
                    (rows : AssocList<string, string> list) : CompilerMonad<PatchFile<'x>, 'env> = 
        compile {
            let! header = getHeaderRow rows
            let! selIds = getSelectionIds rows
            return { PatchType = Download 
                     DataModel = U1
                     EntityType = entityType
                     Variant = ()
                     User = user
                     DateTime = timestamp
                     Selection = selIds
                     HeaderRow = header
                     DataRows = List.map DataRow.FromAssocList rows
                   }          
    }