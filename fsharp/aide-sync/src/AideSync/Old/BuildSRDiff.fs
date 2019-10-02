﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync.Old


module BuildSRDiff =

    

    open SLSqlite.Core

    open AideSync.Old.Datatypes
    open AideSync.Old.DiffImplementation
    open AideSync.Old.BasicQueries




    type NameChange = 
        { Reference : string 
          OldName : string
          NewName : string
        }

    let internal nameChanges1 (lefts : AiStructureItem list)
                              (rights : AideStructureItem list) : NameChange list = 
        let rightsMap = 
            rights 
                |> List.map (fun (x:AideStructureItem) -> (x.Reference, x.CommonName))
                |> Map.ofList
        
        let chooser (item : AiStructureItem) : NameChange option = 
            match Map.tryFind item.Reference rightsMap with
            | None -> None
            | Some name2 -> 
                if item.CommonName <> name2 then
                    Some { Reference = item.Reference
                         ; OldName = item.CommonName
                         ; NewName = name2 }
                else None
        List.choose chooser lefts


    let nameChanges (changeReqId : int64) 
                    (assetId : int64) : SqliteDb<NameChange list>= 
        sqliteDb { 
            let! ai = findAiHierarchy assetId
            let! aide = findAideHierarchy changeReqId assetId
            return (nameChanges1 ai.Items aide.Items)
        }
        
    


