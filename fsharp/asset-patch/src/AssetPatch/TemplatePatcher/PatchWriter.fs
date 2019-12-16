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

  

    let private genFileName (directory : string) 
                            (filePrefix : string) 
                            (namePart : string) : CompilerMonad<string> = 
        compile {
            let name1 = 
                sprintf "%s_%s.txt" (safeName filePrefix) (safeName namePart)
            return Path.Combine(directory, name1)
        }      


    /// At least one row exists 
    let private getHeaderRow (rows : AssocList<string, string> list) : CompilerMonad<HeaderRow> = 
        match rows with
        | [] -> throwError "getHeaderRow - empty list"
        | row1 :: _ -> row1 |> AssocList.keys |> HeaderRow |> mreturn


    let private makeHeader (entityType : EntityType) 
                            (user : string) 
                            (variantName : string)
                            (timestamp : DateTime) : CompilerMonad<FileHeader> = 
        compile {
            return { 
                FileType = Upload 
                DataModel = U1
                EntityType = entityType
                Variant = variantName
                User = user
                DateTime = timestamp 
            }
        }

    let private makeChangeFile (entityType : EntityType) 
                                (variantName : string)
                                (rows : AssocList<string, string> list) : CompilerMonad<ChangeFile> = 
        compile {
            let! user = asks (fun x -> x.UserName)
            let timestamp = DateTime.Now
            let! headerRow = getHeaderRow rows
            let! header = makeHeader entityType user variantName timestamp 
            return { 
                Header = header
                Selection = None
                HeaderDescriptions = getHeaderDescriptions entityType headerRow |> Some
                HeaderRow = headerRow
                DataRows = List.map DataRow.FromAssocList rows 
            }          
        }

    let private writeChangesAndHeaders (outputPath: string)
                                           (changeFile : ChangeFile) : CompilerMonad<unit> =
        compile {
            do! liftAction (fun () -> writePatchAndVariantHeaders outputPath changeFile)
            return ()
        }


    // ************************************************************************
    // New FuncLocs file

    /// Render a list of new FuncLocs into a ChangeFile
    let private makeNewFuncLocsFile (rows : NewFuncLoc list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.FunctionLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeChangeFile FuncLoc "Asset Patch Create FuncLocs"

    /// Write a list of new FuncLocs to a ChangeFile
    let writeNewFuncLocsFile (directory : string) 
                                (filePrefix : string) 
                                (funcLocs : NewFuncLoc list) : CompilerMonad<unit> = 
        compile { 
            match funcLocs with
            | [] -> return ()
            | _ -> 
                let! changes = makeNewFuncLocsFile funcLocs
                let! outPath = genFileName directory filePrefix "01_create_flocs"
                do! writeChangesAndHeaders outPath changes
                return ()
            }

    // ************************************************************************
    // Link FuncLocs file

    /// Render a list of FuncLoc hierarchy changes into a ChangeFile
    let private makeLinkFuncLocsFile (rows : NewFuncLoc list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.FunctionLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())     
            |> makeChangeFile FuncLoc "Asset Patch Link FuncLocs"

    /// Write a list of FuncLoc hierarchy changes to a ChangeFile
    let writeLinkFuncLocsFile (directory : string) 
                                (filePrefix : string) 
                                (funcLocs : NewFuncLoc list) : CompilerMonad<unit> = 
        compile { 
            match funcLocs with
            | [] -> return ()
            | _ -> 
                let! changes = makeNewFuncLocsFile funcLocs
                let! outPath = genFileName directory filePrefix "01_create_flocs"
                do! writeChangesAndHeaders outPath changes
                return ()
            }

    // ************************************************************************
    // New ClassFloc file

    /// Render a list of new ClassFlocs into a ChangeFile
    let private makeNewClassFlocsFile (rows : NewClassFloc list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.Class + "!" + row.FuncLoc.ToString())
            |> List.map (fun x -> x.ToAssocs())    
            |> makeChangeFile ClassFloc "Asset Patch Create ClassFlocs"

    /// Write a list of new ClassFlocs to a ChangeFile
    let writeNewClassFlocsFile (directory : string) 
                                (filePrefix : string) 
                                (classFlocs : NewClassFloc list) : CompilerMonad<unit> = 
        compile { 
            match classFlocs with
            | [] -> return ()
            | _ -> 
                let! changes = makeNewClassFlocsFile classFlocs
                let! outPath = genFileName directory filePrefix "03_create_classflocs"
                do! writeChangesAndHeaders outPath changes
                return ()
            }

    // ************************************************************************
    // New ValuaFloc file

    /// Render a list of new ValuaFloc into a ChangeFile
    let private makeNewValuaFlocsFile (rows : NewValuaFloc list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.FuncLoc.ToString() + "!" + row.CharacteristicID)
            |> List.map (fun x -> x.ToAssocs())       
            |> makeChangeFile ValuaFloc "Asset Patch Create ValuaFlocs"


    /// Write a list of new ValuaFloc to a ChangeFile
    let writeNewValuaFlocsFile (directory : string) 
                            (filePrefix : string) 
                            (valuaFlocs : NewValuaFloc list) : CompilerMonad<unit> = 
        compile { 
            match valuaFlocs with
            | [] -> return ()
            | _ -> 
                let! changes = makeNewValuaFlocsFile valuaFlocs
                let! outPath = genFileName directory filePrefix "04_create_valuaflocs"
                do! writeChangesAndHeaders outPath changes
                return ()
            }


    // ************************************************************************
    // New Equi file

    /// Render a list of new equipment into a ChangeFile
    let private makeNewEquisFile (rows : NewEqui list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.FuncLoc.ToString())
            |> List.map (fun x -> x.ToAssocs())       
            |> makeChangeFile Equi "Asset Patch Create Equis"

    /// Write a list of new Equis to a ChangeFile
    let writeNewEquisFile (directory : string) 
                            (filePrefix : string) 
                            (equis : NewEqui list) : CompilerMonad<unit> = 
        compile { 
            match equis with
            | [] -> return ()
            | _ -> 
                let! changes = makeNewEquisFile equis
                let! outPath = genFileName directory filePrefix "05_create_equipment"
                do! writeChangesAndHeaders outPath changes
                return ()
            }


    // ************************************************************************
    // New ClassEqui file


    /// Render a list of new ClassEquis into a ChangeFile
    let private makeNewClassEquisFile (rows : NewClassEqui list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> (sprintf "%012d" row.EquipmentId) + row.Class)
            |> List.map (fun x -> x.ToAssocs())       
            |> makeChangeFile ClassEqui "Asset Patch Create ClassEquis"

    /// Write a list of new ClassEquis to a ChangeFile
    let writeNewClassEquisFile (directory : string) 
                                (filePrefix : string) 
                                (classEquis : NewClassEqui list) : CompilerMonad<unit> = 
        compile { 
            match classEquis with
            | [] -> return ()
            | _ -> 
                let! changes = makeNewClassEquisFile classEquis
                let! outPath = genFileName directory filePrefix "06_create_classequis"
                do! writeChangesAndHeaders outPath changes
                return ()
            }

    // ************************************************************************
    // New ValuaEqui file

    /// Render a list of new ValuaEquis into a ChangeFile
    let private makeNewValuaEquisFile (rows : NewValuaEqui list) : CompilerMonad<ChangeFile> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentId)
            |> List.map (fun x -> x.ToAssocs())       
            |> makeChangeFile ValuaEqui "Asset Patch Create ValuaEquis"

    /// Write a list of new ValuaEquis to a ChangeFile
    let writeNewValuaEquisFile (directory : string) 
                            (filePrefix : string) 
                            (valuaEquis : NewValuaEqui list) : CompilerMonad<unit> = 
        compile { 
            match valuaEquis with
            | [] -> return ()
            | _ ->       
                let! changes = makeNewValuaEquisFile valuaEquis
                let! outPath = genFileName directory filePrefix "07_create_valuaequis"
                do! writeChangesAndHeaders outPath changes
                return ()
            }

    