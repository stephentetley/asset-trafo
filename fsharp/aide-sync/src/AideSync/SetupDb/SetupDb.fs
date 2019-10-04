// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module SetupDb =
    
    open System.IO

    open SLSqlite.Core
    
    open AideSync.SetupDb.Internal.PopulateDb

    type SetupDbConfig = 
        { ExportsSourceFolder : string
          DbTemplatePath : string
          DbOutputPath : string
        }
   

    let setupDb (config : SetupDbConfig) : Result<unit, ErrMsg> = 
        let sourceCsv (fileName : string) : string = 
            Path.Combine(config.ExportsSourceFolder, fileName)

        let workSchemeCsv       = sourceCsv "change_requests_schemes.csv"
        let changeRequestsCsv   = sourceCsv "change_requests_change_reqs.csv"
        let aiAssetsCsv         = sourceCsv "ai_asset.csv"
        let aideAssetsCsv       = sourceCsv "aide_asset.csv"
        let assetChangesCsv     = sourceCsv "change_requests_assets.csv"
        let attrChangesCsv      = sourceCsv "change_requests_attributes.csv"
        let repAttrChangesCsv   = sourceCsv "change_requests_repeated_attributes.csv"
        let newAssetsCsv        = sourceCsv "no_change_req_new_aide_assets.csv"
        let newAttributesCsv    = sourceCsv "no_change_req_new_aide_attribute_values.csv"
        let aideStructRelsCsv   = sourceCsv "structure_relationships_aide.csv"
        let aiStructRelsCsv     = sourceCsv "structure_relationships_ai.csv"
    
        
        if File.Exists(config.DbOutputPath) then
            System.IO.File.Delete config.DbOutputPath
        else ()
        System.IO.File.Copy(sourceFileName = config.DbTemplatePath, 
                            destFileName = config.DbOutputPath)
    
        let connParams = sqliteConnParamsVersion3 config.DbOutputPath
        runSqliteDb connParams 
            <| sqliteDb { 
                do! insertWorkSchemeRows workSchemeCsv
                do! insertChangeRequestRows changeRequestsCsv
                do! insertAiAssetRows aiAssetsCsv
                do! insertAideAssetRows aideAssetsCsv
                do! insertAssetChangeRows assetChangesCsv
                do! insertAssetNewRows newAssetsCsv
                do! insertAttributeChangeRows attrChangesCsv
                do! insertAttributeNewRows newAttributesCsv
                do! insertRepeatedAttributeChangeRows repAttrChangesCsv
                do! insertAideStructRelationshipRows aideStructRelsCsv
                do! insertAiStructRelationshipRows aiStructRelsCsv
                return ()
            }