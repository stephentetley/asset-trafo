// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.StructureRelationships


module SRDiff =

    open SLSqlite.Core

    open AssetSync.Base.SimpleDiff
    open AssetSync.StructureRelationships.BasicQueries

    let sturctureRelationshipsDiff (changeReqId : int64) 
                                   (sairef : string) : SqliteDb<Differences>= 
        sqliteDb { 
            let! ai = findAiHierarchy sairef
            let! aide = findAideHierarchy changeReqId sairef
            return (diffLists ai.CommonNames aide.CommonNames)
        }



    let nameChanges (changeReqId : int64) 
                    (sairef : string) : SqliteDb<Differences>= 
        sqliteDb { 
            let! ai = findAiHierarchy sairef
            let! aide = findAideHierarchy changeReqId sairef
            return (diffLists ai.CommonNames aide.CommonNames)
        }
        