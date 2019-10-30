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
    open AssetPatch.Base.Printer
    open AssetPatch.Base.EntityTypes


    let outputFileName (outputDirectory : string) 
                        (entityType: EntityType) 
                        (root : string) : string = 
        let name1 = 
            match entityType with
            | FuncLoc -> sprintf "%s_funclocs.txt" (safeName root)
            | ClassFloc -> sprintf "%s_classflocs.txt" (safeName root)
            | ValuaFloc -> sprintf "%s_valuaflocs.txt" (safeName root)
            | Equi -> sprintf "%s_equi.txt" (safeName root)
            | ClassEqui -> sprintf "%s_classequi.txt" (safeName root)
            | ValuaEqui -> sprintf "%s_valuaequi.txt" (safeName root)
        Path.Combine(outputDirectory, name1)




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

    let private makeChangeFile (entityType : EntityType) 
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

    // Generate upload change files for each entity type...

    /// Compile a list for FuncLoc changes into a ChangeFile
    let compileFuncLocFile (user : string) 
                             (timestamp : System.DateTime)
                             (rows : FuncLoc list) : CompilerMonad<ChangeFile, 'env, 'acc> = 
        rows
            |> List.sortBy (fun row -> row.Path.ToString()) 
            |> List.map funcLocToAssocs     
            |> makeChangeFile FuncLoc user timestamp


    /// Compile a list for ClassFloc changes into a ChangeFile
    let compileClassFlocFile (user : string) 
                             (timestamp : System.DateTime)
                             (rows : ClassFloc list) : CompilerMonad<ChangeFile, 'env, 'acc> = 
        rows
            |> List.sortBy (fun row -> row.Class + "!" + row.FuncLoc.ToString())
            |> List.map classFlocToAssocs     
            |> makeChangeFile ClassFloc user timestamp


    /// Compile a list for ValuaFloc changes into a ChangeFile
    let compileValuaFlocFile (user : string) 
                             (timestamp : System.DateTime)
                             (rows : ValuaFloc list) : CompilerMonad<ChangeFile, 'env, 'acc> = 
        rows
            |> List.sortBy (fun row -> row.FuncLoc.ToString() + "!" + row.CharacteristicID)
            |> List.map valuaFlocToAssocs     
            |> makeChangeFile ValuaFloc user timestamp


    /// Compile a list for ClassEqui changes into a ChangeFile
    let compileEquiFile (user : string) 
                        (timestamp : System.DateTime)
                         (rows : Equi list) : CompilerMonad<ChangeFile, 'env, 'acc> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentNumber.ToString())
            |> List.map equiToAssocs     
            |> makeChangeFile Equi user timestamp


    /// Compile a list for ClassEqui changes into a ChangeFile
    let compileClassEquiFile (user : string) 
                             (timestamp : System.DateTime)
                             (rows : ClassEqui list) : CompilerMonad<ChangeFile, 'env, 'acc> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentNumber.ToString() + row.Class)
            |> List.map classEquiToAssocs     
            |> makeChangeFile ClassEqui user timestamp


    /// Compile a list for ValuaEqui changes into a ChangeFile
    let compileValuaEquiFile (user : string) 
                             (timestamp : System.DateTime)
                             (rows : ValuaEqui list) : CompilerMonad<ChangeFile, 'env, 'acc> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentNumber)
            |> List.map valuaEquiToAssocs     
            |> makeChangeFile ValuaEqui user timestamp

    let writeChangeFileAndMetadata (outputDirectory : string) 
                                    (namePrefix : string)
                                    (changeFile : ChangeFile) : CompilerMonad<unit, 'env, 'acc> =
        compile {
            let outpath = outputFileName outputDirectory changeFile.Header.EntityType namePrefix
            let name1 = Path.GetFileNameWithoutExtension(outpath) + ".variant.txt"
            let variantPath = Path.Combine(Path.GetDirectoryName(outpath), name1)        
            writeReceipt variantPath changeFile
            writeChangeFile outpath changeFile
            return ()
        }