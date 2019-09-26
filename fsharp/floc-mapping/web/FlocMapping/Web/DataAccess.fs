// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Web


module DataAccess = 
    
    open System.IO

    open SLSqlite.Core
    
    open FlocMapping.S4Basis
    open FlocMapping.TranslateFloc
    open FlocMapping.Web.Model


    let private pathToDb () : string = 
        Path.Combine(__SOURCE_DIRECTORY__, @"..\..\..\data\db\floc_mapping_active.sqlite")
    
    let private  getConnParams () : SqliteConnParams = 
        let dbActive = pathToDb () |> Path.GetFullPath
        sqliteConnParamsVersion3 dbActive
    
    
    let runDb (action : SqliteDb<'a>) : Result<'a, ErrMsg> = 
        let conn = getConnParams () 
        runSqliteDb conn action


    let s4FlocName (floc : Floc) : SqliteDb<string> = 
        getS4FlocName floc |>> Option.defaultValue ""


    let saiFlocMapping (sai : string) : SqliteDb<LookupAnswer list> = 
        aibReferenceToS4Floc sai >>= 
            mapM (fun x -> s4FlocName x |>> fun name -> FlocAns { S4Floc = x; Description = name } )

    let pliFlocMapping (pli : string) : SqliteDb<LookupAnswer list> = 
        sqliteDb { 
            let s4num = s4EquipmentReference pli |>> Option.defaultValue 0

        }