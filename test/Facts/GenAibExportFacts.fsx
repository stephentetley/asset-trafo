// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.0.1\lib\netstandard2.0"
#r @"FSharp.Data.dll"
open FSharp.Data

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat"
open SLFormat.CommandOptions

#I @"C:\Users\stephen\.nuget\packages\factx\1.0.0-alpha-20190721\lib\netstandard2.0"
#r "FactX"
open FactX
open FactX.FactWriter
open FactX.Skeletons

#I @"C:\Users\stephen\.nuget\packages\slpotassco\1.0.0-alpha-20190723a\lib\netstandard2.0"
#r "SLPotassco"
open SLPotassco.Potassco.Invoke

#load "..\..\src\AssetTrafo\AspFacts\Common.fs"
#load "..\..\src\AssetTrafo\AspFacts\AibAssetTypes.fs"
#load "..\..\src\AssetTrafo\AspFacts\AibRuleTable.fs"
open AssetTrafo.AspFacts.AibAssetTypes
open AssetTrafo.AspFacts.AibRuleTable

let clingoDirectory () = 
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, @"..\..\clingo")


let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\clingo\facts", relativePath)


let ruleTableFacts () = 
    let source = @"G:\work\Projects\asset_sync\rules\ai_rule_table_simple.csv"
    generateFunLocFacts    source (outputFile @"aib_rule_table_funcloc.lp") 
    generateEquipmentFacts source (outputFile @"aib_rule_table_equipment.lp") 
         


let assetTypeFacts () = 
    let source = @"G:\work\Projects\asset_sync\rules\asset_types_groups_categories.csv"
    generateBaseAssetTypeFacts      source (outputFile @"aib_base_asset_type.lp") 
    generateCategoryAndGroupFacts   source (outputFile @"aib_equipment_cats_groups.lp") 

let main () = 
    ruleTableFacts ()
    assetTypeFacts ()