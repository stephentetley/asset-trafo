// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.PatchBuilder



module PatchGen =
   
    open System
    open System.IO

    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.Acronyms
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.Base.Printer
    open AssetPatch.Base.EntityTypes
    
    open AssetPatch.PatchBuilder.Emitter

    let entityName (entityType : EntityType) : string = 
        match entityType with
        | FuncLoc -> "funcloc"
        | ClassFloc  -> "classfloc"
        | ValuaFloc -> "valuafloc"
        | Equi -> "equi"
        | ClassEqui -> "classequi"
        | ValuaEqui -> "valuaequi"
        

    let genFileName (directory : string) 
                    (filePrefix : string) 
                    (entityType : EntityType) : CompilerMonad<string, 'env> = 
        compile {
            let! idx = newFileIndex ()
            let name1 = 
                sprintf "%s_%02i_%s.txt" (safeName filePrefix) idx (entityName entityType)
            return Path.Combine(directory, name1)
        }


    /// At least one row exists 
    let getHeaderRow (rows : AssocList<string, string> list) : CompilerMonad<HeaderRow, 'env> = 
        match rows with
        | [] -> throwError "getHeaderRow - empty list"
        | row1 :: _ -> row1 |> AssocList.keys |> HeaderRow |> mreturn


    let makeHeader (entityType : EntityType) 
                    (user : string) 
                    (timestamp : DateTime) : CompilerMonad<FileHeader, 'env> = 
        mreturn { 
            FileType = Upload 
            DataModel = U1
            EntityType = entityType
            Variant = ()
            User = user
            DateTime = timestamp 
        }

    let private makeChangeFile (entityType : EntityType) 
                                (user : string) 
                                (timestamp : System.DateTime)
                                (rows : AssocList<string, string> list) : CompilerMonad<ChangeFile, 'env> = 
        compile {
            let! headerRow = getHeaderRow rows
            let! header = makeHeader entityType user timestamp 
            return { 
                Header = header
                Selection = None
                HeaderDescriptions = getHeaderDescriptions entityType headerRow |> Some
                HeaderRow = headerRow
                DataRows = List.map DataRow.FromAssocList rows 
            }          
        }

    let writeChangeFileAndMetadata (outputPath: string)
                                   (changeFile : ChangeFile) : CompilerMonad<unit, 'env> =
        compile {
            let variant1 = Path.GetFileNameWithoutExtension(outputPath) + ".variant.txt"
            let variantPath = Path.Combine(Path.GetDirectoryName(outputPath), variant1)        
            writeChangeFile outputPath changeFile
            writeReceipt variantPath changeFile            
            return ()
        }

    /// Compile a list for ClassEqui changes into a ChangeFile
    let compileEquiFile (user : string) 
                        (timestamp : System.DateTime)
                        (rows : Equi list) : CompilerMonad<ChangeFile, 'env> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentNumber.ToString())
            |> List.map equiToAssocs     
            |> makeChangeFile Equi user timestamp


    let genEquiFile (directory : string) 
                    (filePrefix : string) 
                    (user: string) 
                    (equis : Equi list) : CompilerMonad<unit, 'env> = 
        compile { 
            match equis with
            | [] -> return ()
            | _ -> 
                let! equiChanges = compileEquiFile user DateTime.Now equis
                let! outPath = genFileName directory filePrefix Equi
                do! writeChangeFileAndMetadata outPath equiChanges
                return ()
            }

    let generatePatches (directory : string) 
                        (filePrefix : string) 
                        (user: string) 
                        (results : EmitterResults) : CompilerMonad<unit, 'env> = 
        compile {
            do! genEquiFile directory filePrefix user results.Equis
            return ()
        }

