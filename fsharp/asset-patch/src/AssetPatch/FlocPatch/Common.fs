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
    open AssetPatch.Base.EntityTypes


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