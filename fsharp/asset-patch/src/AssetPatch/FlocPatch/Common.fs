// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocPatch



module Common =
    
    open System.IO

    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.Acronyms
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
    let getHeaderRow (rows : AssocList<string, string> list) : CompilerMonad<HeaderRow, 'env, 'acc> = 
        match rows with
        | [] -> throwError "getHeaderRow - empty list"
        | row1 :: _ -> row1 |> AssocList.keys |> HeaderRow |> mreturn





    let private makeHeader (entityType : EntityType) 
                            (user : string) 
                            (timestamp : System.DateTime) : FileHeader = 
        { FileType = Upload 
          DataModel = U1
          EntityType = entityType
          Variant = ()
          User = user
          DateTime = timestamp }

    let makeChangeFile (entityType : EntityType) 
                        (user : string) 
                        (timestamp : System.DateTime)
                        (rows : AssocList<string, string> list) : CompilerMonad<ChangeFile, 'env, 'acc> = 
        compile {
            let! header = getHeaderRow rows
            return { Header = makeHeader entityType user timestamp 
                     Selection = None
                     HeaderDescriptions = 
                        getHeaderDescriptions entityType header |> Some
                     HeaderRow = header
                     DataRows = List.map DataRow.FromAssocList rows }          
        }