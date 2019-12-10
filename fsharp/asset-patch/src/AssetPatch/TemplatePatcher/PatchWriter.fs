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
    open AssetPatch.Base.Printer
    open AssetPatch.TemplatePatcher.CompilerMonad
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

        
    let genSubFolder (directory : string) (level : int) : CompilerMonad<string> =
        let name1  = 
            match level with
            | 1 -> Some "Level_01_sites"
            | 2 -> Some "Level_02_functions"
            | 3 -> Some "Level_03_process_groups"
            | 4 -> Some "Level_04_processes"
            | 5 -> Some "Level_05_systems"
            | 6 -> Some "Level_06_assemblies"
            | 7 -> Some "Level_07_items"
            | 8 -> Some "Level_08_components"
            | _ -> None
        match name1 with
        | None -> throwError "Unknown Level"
        | Some name -> 
                let fullName = Path.Combine(directory, name)
                if not <| IO.Directory.Exists(fullName) then
                    IO.Directory.CreateDirectory(fullName) |> ignore
                else ()
                mreturn fullName

                    

    let private genFileName (directory : string) 
                            (level : int)
                            (filePrefix : string) 
                            (entityType : EntityType) : CompilerMonad<string> = 
        compile {
            let! idx = newFileIndex level
            let! subdirectory = genSubFolder directory level
            let name1 = 
                sprintf "%s_level%i_%02i_%s.%s" (safeName filePrefix) level idx (entityName entityType) (entityExtension entityType)
            return Path.Combine(subdirectory, name1)
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
    let private makeFuncLocFile (rows : PatchFuncLoc list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.Path.ToString()) 
            |> List.map funcLocToAssocs     
            |> makeChangeFile FuncLoc

    /// Write a list of FuncLoc changes to a ChangeFile
    let writeFuncLocFile (directory : string) 
                            (level : int)
                            (filePrefix : string) 
                            (funcLocs : PatchFuncLoc list) : CompilerMonad<unit> = 
        compile { 
            match funcLocs with
            | [] -> return ()
            | _ -> 
                let! changes = makeFuncLocFile funcLocs
                let! outPath = genFileName directory level filePrefix FuncLoc
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    // ************************************************************************
    // FuncLoc file

    /// Render a list of ClassFloc changes into a ChangeFile
    let private makeClassFlocFile (rows : PatchClassFloc list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.Class + "!" + row.FuncLoc.ToString())
            |> List.map classFlocToAssocs     
            |> makeChangeFile ClassFloc

    /// Write a list of ClassFloc changes to a ChangeFile
    let writeClassFlocFile (directory : string) 
                            (level : int)
                            (filePrefix : string) 
                            (classFlocs : PatchClassFloc list) : CompilerMonad<unit> = 
        compile { 
            match classFlocs with
            | [] -> return ()
            | _ -> 
                let! changes = makeClassFlocFile classFlocs
                let! outPath = genFileName directory level filePrefix ClassFloc
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    // ************************************************************************
    // ValuaFloc file

    /// Render a list of ValuaFloc changes into a ChangeFile
    let private makeValuaFlocFile (rows : PatchValuaFloc list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.FuncLoc.ToString() + "!" + row.CharacteristicID)
            |> List.map valuaFlocToAssocs     
            |> makeChangeFile ValuaFloc


    /// Write a list of ValuaFloc changes to a ChangeFile
    let writeValuaFlocFile (directory : string) 
                            (level : int)
                            (filePrefix : string) 
                            (valuaFlocs : PatchValuaFloc list) : CompilerMonad<unit> = 
        compile { 
            match valuaFlocs with
            | [] -> return ()
            | _ -> 
                let! changes = makeValuaFlocFile valuaFlocs
                let! outPath = genFileName directory level filePrefix ValuaFloc
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }


    // ************************************************************************
    // Equi file

    /// Render a list of ClassEqui changes into a ChangeFile
    let private makeEquiFile (rows : PatchEqui list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.InterimId)
            |> List.map equiToAssocs     
            |> makeChangeFile Equi

    /// Write a list of Equi changes to a ChangeFile
    let writeEquiFile (directory : string) 
                        (level : int)
                        (filePrefix : string) 
                        (equis : PatchEqui list) : CompilerMonad<unit> = 
        compile { 
            match equis with
            | [] -> return ()
            | _ -> 
                let! changes = makeEquiFile equis
                let! outPath = genFileName directory level filePrefix Equi
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    /// Write an EquiIndexing file
    let writeEquiIndexing (directory : string) 
                            (level : int)
                            (equis : PatchEqui list) : CompilerMonad<unit> =  
        compile {
            let outputPath = Path.Combine(directory, "EquiIndexing.xlsx")
            return! writeEquiIndexingSheet outputPath equis
            }

    // ************************************************************************
    // ClassEqui file


    /// Render a list of ClassEqui changes into a ChangeFile
    let private makeClassEquiFile (rows : PatchClassEqui list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.InterimId + row.Class)
            |> List.map classEquiToAssocs     
            |> makeChangeFile ClassEqui

    /// Write a list of ClassEqui changes to a ChangeFile
    let writeClassEquiFile (directory : string) 
                            (level : int)
                            (filePrefix : string) 
                            (classEquis : PatchClassEqui list) : CompilerMonad<unit> = 
        compile { 
            match classEquis with
            | [] -> return ()
            | _ -> 
                let! changes = makeClassEquiFile classEquis
                let! outPath = genFileName directory level filePrefix ClassEqui
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    // ************************************************************************
    // ValuaEqui file

    /// Render a list of ValuaEqui changes into a ChangeFile
    let private makeValuaEquiFile (rows : PatchValuaEqui list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.InterimId)
            |> List.map valuaEquiToAssocs     
            |> makeChangeFile ValuaEqui

    /// Write a list of ValuaEqui changes to a ChangeFile
    let writeValuaEquiFile (directory : string) 
                            (level : int)
                            (filePrefix : string) 
                            (valuaEquis : PatchValuaEqui list) : CompilerMonad<unit> = 
        compile { 
            match valuaEquis with
            | [] -> return ()
            | _ ->       
            
                let! changes = makeValuaEquiFile valuaEquis
                let! outPath = genFileName directory level filePrefix ValuaEqui
                do! writeChangeFileAndMetadata outPath changes
                return ()
            }

    