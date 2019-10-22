// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FuncLocBuilder


[<RequireQualifiedAccess>]
module FuncLocPatch =
    
    
    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.Syntax


    let funcLocAssocList (funcLoc : FuncLoc) : AssocList<string,string> option = 
        let magicFields = [ "JOBJN_FL"; "FLOC_REF" ]
        match funcLoc.FuncLocPath.Parent with
        | None -> None
        | Some parent ->             
            funcLoc.InheritedAttributes
                |> AssocList.removes magicFields
                |> AssocList.update "FUNCLOC" (funcLoc.FuncLocPath.ToString())
                |> AssocList.update "TXTMI" funcLoc.Description
                |> AssocList.update "FLTYP" (funcLoc.Level.ToString())
                |> AssocList.update "IEQUI" (if funcLoc.Level >= 5 then "X" else "")
                |> AssocList.update "EQART" funcLoc.ObjectType
                |> AssocList.update "TPLMA1" (parent.ToString())
                |> AssocList.update "TPLMA" (parent.ToString())
                |> Some



    let getHeaderRow (rows : AssocList<string, string> list) : HeaderRow option = 
        match rows with
        | [] -> None
        | row1 :: _ -> row1 |> AssocList.keys |> HeaderRow |> Some

    let getSelectionIds (rows : AssocList<string, string> list) : (SelectionId list) option = 
        let build1 assocs = 
            match AssocList.tryFind "FUNCLOC" assocs with
            | None -> None
            | Some funcloc -> FuncLocEq funcloc |> Some        
        List.map build1 rows |> allSome


    let makePatch (user : string) 
                  (timestamp : System.DateTime)
                  (funcLocs : FuncLoc list) : Result<FuncLocPatch, ErrMsg> = 
        let makeRows = 
            List.sort >> List.map funcLocAssocList >> allSome

        match makeRows funcLocs with
        | None -> Error "Empty or invalid funclocs..."
        | Some rows -> 
            match getHeaderRow rows, getSelectionIds rows with
            | Some header, Some selIds -> 
                Ok { PatchType = Download 
                     DataModel = U1
                     EntityType = FuncLoc
                     Variant = ()
                     User = user
                     DateTime = timestamp
                     Selection = selIds
                     HeaderRow = header
                     DataRows = List.map DataRow.FromAssocList rows
                   }
            | None, _ -> Error "Could not extract headers"
            | _, None -> Error "Could not extract Selection Ids"
                


