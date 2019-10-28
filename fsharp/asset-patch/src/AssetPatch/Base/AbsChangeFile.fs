﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base



module AbsChangeFile =

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
   

    type AbsChangeFile = 
        { Header : FileHeader 
          Rows : AssocList<string, string> list }

        member x.Prioritize (keys : string list) : AbsChangeFile = 
            let rows1 = x.Rows |> List.map (AssocList.prioritize keys) 
            { Header = x.Header; Rows = rows1 }

        member x.Restrict (keys : string list) : AbsChangeFile = 
            let rows1 = x.Rows |> List.map (AssocList.removes keys) 
            { Header = x.Header; Rows = rows1 }

    
    let ofChangeFile (changeFile : ChangeFile) : AbsChangeFile = 
        { Header = changeFile.Header 
          Rows = changeFile.RowAssocs
        }


    let prioritize (keys : string list) (patch : AbsChangeFile) : AbsChangeFile = 
        patch.Prioritize keys

    let restrict (keys : string list) (patch : AbsChangeFile) : AbsChangeFile = 
        patch.Restrict keys



    let private idField (entityType : EntityType) : string = 
        match entityType with
        | FuncLoc | ClassFloc | ValuaFloc -> "FUNCLOC"
        | Equi | ClassEqui | ValuaEqui -> "EQUI"



    /// At least one row exists and it must have a field 
    /// matching ``FUNCLOC`` or ``EQUI``
    let private selectionIds (entityType : EntityType)
                             (rows : AssocList<string, string> list) : Option<SelectionId list> = 
        let key = idField entityType
        let build1 assocs = 
            match AssocList.tryFind key assocs with
            | None -> None
            | Some funcloc -> FuncLocEq funcloc |> Some        
        List.map build1 rows 
            |> allSome


    /// At least one row exists 
    let headerRow (rows : AssocList<string, string> list) : HeaderRow option = 
        match rows with
        | [] -> None
        | row1 :: _ -> row1 |> AssocList.keys |> HeaderRow |> Some


    let toChangeFile (absChangeFile : AbsChangeFile) : Result<ChangeFile, ErrMsg> = 
        match selectionIds absChangeFile.Header.EntityType absChangeFile.Rows, 
                headerRow absChangeFile.Rows with
        | Some selIds, Some header -> 
            Ok { Header = absChangeFile.Header
                 Selection = selIds
                 HeaderRow = header
                 DataRows = List.map DataRow.FromAssocList absChangeFile.Rows
                 }
        | None, _ -> Error "AbsChangeFile - could not extract selection ids"
        | _, None -> Error "AbsChangeFile - could not extract header row"