// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.StructureRelationships


module SRDiff =

    

    open SLSqlite.Core

    open AssetSync.StructureRelationships.Datatypes
    open AssetSync.StructureRelationships.StructureItemDiff
    open AssetSync.StructureRelationships.BasicQueries




    type NameChange = 
        { Reference : string 
          OldName : string
          NewName : string
        }

    let internal nameChanges1 (lefts : StructureItem list)
                              (rights : StructureItem list) : NameChange list = 
        let rightsMap = 
            rights 
                |> List.map (fun (x:StructureItem) -> (x.Reference, x.CommonName))
                |> Map.ofList
        
        let chooser (item : StructureItem) : NameChange option = 
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
                    (sairef : string) : SqliteDb<NameChange list>= 
        sqliteDb { 
            let! ai = findAiHierarchy sairef
            let! aide = findAideHierarchy changeReqId sairef
            return (nameChanges1 ai.Items aide.Items)
        }
        
    



    let sturctureRelationshipsDiff (changeReqId : int64) 
                                   (sairef : string) : SqliteDb<Differences>= 
        sqliteDb { 
            let! ai = findAiHierarchy sairef
            let! aide = findAideHierarchy changeReqId sairef
            return (diffLists ai.Items aide.Items)
        }