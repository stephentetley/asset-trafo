// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher

// This module is a hack because we don't know Equipment number (EQUI)
// until we have activated the Equi patch and downloaded the generated
// EQUI numbers.

module EquiIndexing =
    
    open System.IO
    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.Parser
    open AssetPatch.Base.FuncLocPath

    type EquiIndex = 
        { Equi : uint32 
          Txtmi : string
          TplnEilo : string
        }

    type EquiKey = 
        { Description : string 
          FuncLoc : FuncLocPath }

    type EquiMap = Map<EquiKey, uint32>

    let emptyEquiMap : EquiMap = Map.empty

    let buildEquiMap (indices : EquiIndex list) : EquiMap = 
        let add1 (acc : EquiMap) (ix : EquiIndex) = 
            let key = { Description = ix.Txtmi; FuncLoc = FuncLocPath.Create ix.TplnEilo }
            Map.add key ix.Equi acc
        List.fold add1 Map.empty indices

    let private extractEquiIndex (row : AssocList<string, string>) : EquiIndex option = 
        match AssocList.tryFind3 "EQUI" "TXTMI" "TPLN_EILO" row with
        | Some (a,b,c) -> 
            try 
                let num = uint32 a 
                Some { Equi  = num;  Txtmi = b; TplnEilo = c }
            with
            | _ -> None
        | None -> None
    
    let readEquiDownload (path : string) : Result<EquiMap, ErrMsg> = 
        match readChangeFile path with
        | Error msg -> Error msg
        | Ok changes -> 
            changes 
                |> fun x -> x.RowAssocs()
                |> List.map extractEquiIndex
                |> List.choose id
                |> buildEquiMap
                |> Ok

    let tryFindEquiNum (description : string) 
                (funcLoc : FuncLocPath) 
                (indices : EquiMap) : uint32 option = 
        Map.tryFind { Description = description; FuncLoc = funcLoc } indices