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


    // ************************************************************************
    // FuncLoc file

    /// Compile a list for FuncLoc changes into a ChangeFile
    let compileFuncLocFile (user : string) 
                             (timestamp : System.DateTime)
                             (rows : FuncLoc list) : CompilerMonad<ChangeFile, 'env> = 
        rows
            |> List.sortBy (fun row -> row.Path.ToString()) 
            |> List.map funcLocToAssocs     
            |> makeChangeFile FuncLoc user timestamp

    let genFuncLocFile (directory : string) 
                        (filePrefix : string) 
                        (user : string) 
                        (funcLocs : FuncLoc list) : CompilerMonad<unit, 'env> = 
        compile { 
            match funcLocs with
            | [] -> return ()
            | _ -> 
                let! changes = compileFuncLocFile user DateTime.Now funcLocs
                let! outPath = genFileName directory filePrefix FuncLoc
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    // ************************************************************************
    // FuncLoc file

    /// Compile a list for ClassFloc changes into a ChangeFile
    let compileClassFlocFile (user : string) 
                             (timestamp : System.DateTime)
                             (rows : ClassFloc list) : CompilerMonad<ChangeFile, 'env> = 
        rows
            |> List.sortBy (fun row -> row.Class + "!" + row.FuncLoc.ToString())
            |> List.map classFlocToAssocs     
            |> makeChangeFile ClassFloc user timestamp

    let genClassFlocFile (directory : string) 
                        (filePrefix : string) 
                        (user : string) 
                        (classFlocs : ClassFloc list) : CompilerMonad<unit, 'env> = 
        compile { 
            match classFlocs with
            | [] -> return ()
            | _ -> 
                let! changes = compileClassFlocFile user DateTime.Now classFlocs
                let! outPath = genFileName directory filePrefix ClassFloc
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    // ************************************************************************
    // ValuaFloc file

    /// Compile a list for ValuaFloc changes into a ChangeFile
    let compileValuaFlocFile (user : string) 
                             (timestamp : System.DateTime)
                             (rows : ValuaFloc list) : CompilerMonad<ChangeFile, 'env> = 
        rows
            |> List.sortBy (fun row -> row.FuncLoc.ToString() + "!" + row.CharacteristicID)
            |> List.map valuaFlocToAssocs     
            |> makeChangeFile ValuaFloc user timestamp


    let genValuaFlocFile (directory : string) 
                        (filePrefix : string) 
                        (user : string) 
                        (valuaFlocs : ValuaFloc list) : CompilerMonad<unit, 'env> = 
        compile { 
            match valuaFlocs with
            | [] -> return ()
            | _ -> 
                let! changes = compileValuaFlocFile user DateTime.Now valuaFlocs
                let! outPath = genFileName directory filePrefix ValuaFloc
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }


    // ************************************************************************
    // Equi file

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
                let! changes = compileEquiFile user DateTime.Now equis
                let! outPath = genFileName directory filePrefix Equi
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    // ************************************************************************
    // ClassEqui file


    /// Compile a list for ClassEqui changes into a ChangeFile
    let compileClassEquiFile (user : string) 
                             (timestamp : System.DateTime)
                             (rows : ClassEqui list) : CompilerMonad<ChangeFile, 'env> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentNumber.ToString() + row.Class)
            |> List.map classEquiToAssocs     
            |> makeChangeFile ClassEqui user timestamp

    let genClassEquiFile (directory : string) 
                        (filePrefix : string) 
                        (user: string) 
                        (classEquis : ClassEqui list) : CompilerMonad<unit, 'env> = 
        compile { 
            match classEquis with
            | [] -> return ()
            | _ -> 
                let! changes = compileClassEquiFile user DateTime.Now classEquis
                let! outPath = genFileName directory filePrefix ClassEqui
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    // ************************************************************************
    // ValuaEqui file

    /// Compile a list for ValuaEqui changes into a ChangeFile
    let compileValuaEquiFile (user : string) 
                             (timestamp : System.DateTime)
                             (rows : ValuaEqui list) : CompilerMonad<ChangeFile, 'env> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentNumber)
            |> List.map valuaEquiToAssocs     
            |> makeChangeFile ValuaEqui user timestamp


    let genValuaEquiFile (directory : string) 
                        (filePrefix : string) 
                        (user: string) 
                        (valuaEquis : ValuaEqui list) : CompilerMonad<unit, 'env> = 
        compile { 
            match valuaEquis with
            | [] -> return ()
            | _ -> 
                let! changes = compileValuaEquiFile user DateTime.Now valuaEquis
                let! outPath = genFileName directory filePrefix ValuaEqui
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    // ************************************************************************
    // Generate patches

    let generatePatches (directory : string) 
                        (filePrefix : string) 
                        (user : string) 
                        (results : EmitterResults) : CompilerMonad<unit, 'env> = 
        compile {
            do! genFuncLocFile   directory filePrefix user results.FuncLocs
            do! genClassFlocFile directory filePrefix user results.ClassFlocs
            do! genValuaFlocFile directory filePrefix user results.ValuaFlocs
            do! genEquiFile      directory filePrefix user results.Equis
            do! genClassEquiFile directory filePrefix user results.ClassEquis
            do! genValuaEquiFile directory filePrefix user results.ValuaEquis
            return ()
        }

