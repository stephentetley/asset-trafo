// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FuncLocBuilder


[<AutoOpen>]
module FuncLocType =
    
    open AssetPatch.Base
    open AssetPatch.FuncLocBuilder
    
    /// The other way is to look at differences to an existing root funcloc
    /// Then only 8 fields change:
    ///
    /// 1   FUNCLOC
    /// 2   TXTMI
    /// 38  FLTYP
    /// 42  IEQUI
    /// 56  FLOC_REF  {- Magic -}
    /// 62  EQART
    /// 63  JOBJN_FL  {- Magic -}
    /// 94  TPLMA1
    /// 95  TPLMA
    
    
    
    
    type FuncLoc = 
        { 
            FuncLocPath : FuncLocPath
            Description : string 
            ObjectType : string
            InheritedAttributes : AssocList<string,string>
        }
            
        member x.Level with get () : int = x.FuncLocPath.Level

        static member Initial (attributes : AssocList<string, string>) : FuncLoc option = 
            match AssocList.tryFind "FUNCLOC" attributes, 
                    AssocList.tryFind "TXTMI" attributes, 
                    AssocList.tryFind "EQART" attributes with
            | Some funcloc, Some desc, Some otype -> 
                Some { FuncLocPath = FuncLocPath.Create funcloc 
                       Description = desc
                       ObjectType = otype
                       InheritedAttributes = attributes
                      }
             | _,_,_ -> None

                
            
    
    
[<RequireQualifiedAccess>]
module FuncLoc =
    
    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.Syntax
    open AssetPatch.Base.Parser


    let getRootFromPathFile (rootCode : string) (filePath : string) : Result<FuncLoc, string> = 
        match readPatch filePath with
        | Result.Error msg -> failwith msg
        | Result.Ok ans ->
            match ans.TryFindAssoc (fun key value -> key = "FUNCLOC" && value = rootCode) with
            | None -> Result.Error (sprintf "Could not find root %s" rootCode)
            | Some ans -> 
                match FuncLoc.Initial ans with 
                | Some floc -> Result.Ok floc
                | None -> Result.Error "Error reading FuncLoc attributes"

                

    let extend (itemCode : string) 
                (description : string) 
                (objType: string) 
                (floc: FuncLoc) : FuncLoc = 
        { FuncLocPath = FuncLocPath.extend itemCode floc.FuncLocPath
          Description = description
          ObjectType = objType
          InheritedAttributes = floc.InheritedAttributes }

    let toPatchData (funcloc : FuncLoc) : AssocList<string,string> option = 
        let magicFields = [ "JOBJN_FL"; "FLOC_REF" ]
        match FuncLocPath.parent funcloc.FuncLocPath with
        | None -> None
        | Some parent ->             
            funcloc.InheritedAttributes
                |> AssocList.removes magicFields
                |> AssocList.update "FUNCLOC" (funcloc.FuncLocPath.ToString())
                |> AssocList.update "TXTMI" funcloc.Description
                |> AssocList.update "FLTYP" (funcloc.Level.ToString())
                |> AssocList.update "IEQUI" (if funcloc.Level >= 5 then "X" else "")
                |> AssocList.update "EQART" funcloc.ObjectType
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


    let makeAdditionsPatch (user : string) 
                            (timestamp : System.DateTime)
                            (funcLocs : FuncLoc list) : Result<PatchFile, ErrMsg> = 
        let makeRows = 
            List.sort >> List.map toPatchData >> allSome

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
                


