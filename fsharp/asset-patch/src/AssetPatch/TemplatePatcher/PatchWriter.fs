// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module PatchWriter =
   
    open System
    open System.IO

    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.Acronyms
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.Base.Printer
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.EquiIndexing

    let private entityName (entityType : EntityType) : string = 
        match entityType with
        | FuncLoc -> "funcloc"
        | ClassFloc  -> "classfloc"
        | ValuaFloc -> "valuafloc"
        | Equi -> "equi"
        | ClassEqui -> "classequi"
        | ValuaEqui -> "valuaequi"

    let private entityExtension (entityType : EntityType) : string = 
        match entityType with
        | FuncLoc | ClassFloc | ValuaFloc | Equi -> "txt"
        | ClassEqui | ValuaEqui -> "apch"

        

    let private genFileName (directory : string) 
                            (filePrefix : string) 
                            (entityType : EntityType) : CompilerMonad<string> = 
        compile {
            let! idx = newFileIndex ()
            let name1 = 
                sprintf "%s_%02i_%s.%s" (safeName filePrefix) idx (entityName entityType) (entityExtension entityType)
            return Path.Combine(directory, name1)
        }

    let getVariant (entityType : EntityType) : CompilerMonad<string option> = 
        match entityType with
        | FuncLoc -> asks (fun x -> x.FlocVariant)
        | Equi -> asks (fun x -> x.EquiVariant)
        | _ -> mreturn None
        


    /// At least one row exists 
    let private getHeaderRow (rows : AssocList<string, string> list) : CompilerMonad<HeaderRow> = 
        match rows with
        | [] -> throwError "getHeaderRow - empty list"
        | row1 :: _ -> row1 |> AssocList.keys |> HeaderRow |> mreturn


    let private makeHeader (entityType : EntityType) 
                            (user : string) 
                            (timestamp : DateTime) : CompilerMonad<FileHeader> = 
        compile {
            let! variantName = getVariant entityType
            return { 
                FileType = Upload 
                DataModel = U1
                EntityType = entityType
                Variant = Option.defaultValue "" variantName
                User = user
                DateTime = timestamp 
            }
        }

    let private makeChangeFile (entityType : EntityType) 
                               (rows : AssocList<string, string> list) : CompilerMonad<ChangeFile> = 
        compile {
            let! user = asks (fun x -> x.UserName)
            let timestamp = DateTime.Now
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

    let private writeChangeFileAndMetadata (outputPath: string)
                                           (changeFile : ChangeFile) : CompilerMonad<unit> =
        compile {
            let variant1 = Path.GetFileNameWithoutExtension(outputPath) + ".variant.txt"
            let variantPath = Path.Combine(Path.GetDirectoryName(outputPath), variant1)        
            writeChangeFile outputPath changeFile
            writeReceipt variantPath changeFile            
            return ()
        }


    // ************************************************************************
    // FuncLoc file

    /// Render a list of FuncLoc changes into a ChangeFile
    let private makeFuncLocFile (rows : FuncLoc list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.Path.ToString()) 
            |> List.map funcLocToAssocs     
            |> makeChangeFile FuncLoc

    let writeFuncLocFile (directory : string) 
                            (filePrefix : string) 
                            (funcLocs : FuncLoc list) : CompilerMonad<unit> = 
        compile { 
            match funcLocs with
            | [] -> return ()
            | _ -> 
                let! changes = makeFuncLocFile funcLocs
                let! outPath = genFileName directory filePrefix FuncLoc
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    // ************************************************************************
    // FuncLoc file

    /// Render a list of ClassFloc changes into a ChangeFile
    let private makeClassFlocFile (rows : ClassFloc list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.Class + "!" + row.FuncLoc.ToString())
            |> List.map classFlocToAssocs     
            |> makeChangeFile ClassFloc


    let writeClassFlocFile (directory : string) 
                            (filePrefix : string) 
                            (classFlocs : ClassFloc list) : CompilerMonad<unit> = 
        compile { 
            match classFlocs with
            | [] -> return ()
            | _ -> 
                let! changes = makeClassFlocFile classFlocs
                let! outPath = genFileName directory filePrefix ClassFloc
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    // ************************************************************************
    // ValuaFloc file

    /// Render a list of ValuaFloc changes into a ChangeFile
    let private makeValuaFlocFile (rows : ValuaFloc list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.FuncLoc.ToString() + "!" + row.CharacteristicID)
            |> List.map valuaFlocToAssocs     
            |> makeChangeFile ValuaFloc


    let writeValuaFlocFile (directory : string) 
                            (filePrefix : string) 
                            (valuaFlocs : ValuaFloc list) : CompilerMonad<unit> = 
        compile { 
            match valuaFlocs with
            | [] -> return ()
            | _ -> 
                let! changes = makeValuaFlocFile valuaFlocs
                let! outPath = genFileName directory filePrefix ValuaFloc
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }


    // ************************************************************************
    // Equi file

    /// Render a list of ClassEqui changes into a ChangeFile
    let private makeEquiFile (rows : Equi list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentNumber.ToString())
            |> List.map equiToAssocs     
            |> makeChangeFile Equi


    let writeEquiFile (directory : string) 
                        (filePrefix : string) 
                        (equis : Equi list) : CompilerMonad<unit> = 
        compile { 
            match equis with
            | [] -> return ()
            | _ -> 
                let! changes = makeEquiFile equis
                let! outPath = genFileName directory filePrefix Equi
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }


    let writeEquiIndexing (directory : string) 
                        (equis : Equi list) : CompilerMonad<unit> =  
        compile {
            let outputPath = Path.Combine(directory, "EquiIndexing.xlsx")
            return! writeEquiIndexingSheet outputPath equis
            }

    // ************************************************************************
    // ClassEqui file


    /// Render a list of ClassEqui changes into a ChangeFile
    let private makeClassEquiFile (rows : ClassEqui list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentNumber.ToString() + row.Class)
            |> List.map classEquiToAssocs     
            |> makeChangeFile ClassEqui

    let writeClassEquiFile (directory : string) 
                            (filePrefix : string) 
                            (classEquis : ClassEqui list) : CompilerMonad<unit> = 
        compile { 
            match classEquis with
            | [] -> return ()
            | _ -> 
                let! changes = makeClassEquiFile classEquis
                let! outPath = genFileName directory filePrefix ClassEqui
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    // ************************************************************************
    // ValuaEqui file

    /// Render a list of ValuaEqui changes into a ChangeFile
    let private makeValuaEquiFile (rows : ValuaEqui list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentNumber)
            |> List.map valuaEquiToAssocs     
            |> makeChangeFile ValuaEqui


    let writeValuaEquiFile (directory : string) 
                            (filePrefix : string) 
                            (valuaEquis : ValuaEqui list) : CompilerMonad<unit> = 
        compile { 
            match valuaEquis with
            | [] -> return ()
            | _ ->       
            
                let! changes = makeValuaEquiFile valuaEquis
                let! outPath = genFileName directory filePrefix ValuaEqui
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    