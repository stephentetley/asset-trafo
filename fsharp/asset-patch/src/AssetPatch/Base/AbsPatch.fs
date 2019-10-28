// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base


[<AutoOpen>]
module AbsPatchType =

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
   

    type AbsPatch = 
        { Header : FileHeader 
          Rows : AssocList<string, string> list }

        member x.Prioritize (keys : string list) : AbsPatch = 
            let rows1 = x.Rows |> List.map (AssocList.prioritize keys) 
            { Header = x.Header; Rows = rows1 }

        member x.Restrict (keys : string list) : AbsPatch = 
            let rows1 = x.Rows |> List.map (AssocList.removes keys) 
            { Header = x.Header; Rows = rows1 }


[<RequireQualifiedAccess>]
module AbsPatch =
    
    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
    
    let ofPatchFile (patch : ChangeFile<'any>) : AbsPatch = 
        { Header = patch.Header 
          Rows = patch.RowAssocs
        }


    let prioritize (keys : string list) (patch : AbsPatch) : AbsPatch = 
        patch.Prioritize keys

    let restrict (keys : string list) (patch : AbsPatch) : AbsPatch = 
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


    let toPatchFile (absPatch : AbsPatch) : Result<ChangeFile<'any> , ErrMsg> = 
        match selectionIds absPatch.Header.EntityType absPatch.Rows, 
                headerRow absPatch.Rows with
        | Some selIds, Some header -> 
            Ok { Header = absPatch.Header
                 Selection = selIds
                 HeaderRow = header
                 DataRows = List.map DataRow.FromAssocList absPatch.Rows
                 }
        | None, _ -> Error "AbsPatch - could not extract selection ids"
        | _, None -> Error "AbsPatch - could not extract header row"