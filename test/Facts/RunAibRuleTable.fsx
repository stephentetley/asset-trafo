// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.0.1\lib\netstandard2.0"
#r @"FSharp.Data.dll"
open FSharp.Data


#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat"

#I @"C:\Users\stephen\.nuget\packages\factx\1.0.0-alpha-20190721\lib\netstandard2.0"
#r "FactX"
open FactX
open FactX.FactWriter
open FactX.Skeletons

#load "..\..\src\AssetTrafo\AspFacts\Common.fs"
#load "..\..\src\AssetTrafo\AspFacts\AibRuleTable.fs"
open AssetTrafo.AspFacts.AibRuleTable


let localFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, "..\..", relativePath)



let main () = 
    let inputFile = @"G:\work\Projects\asset_sync\rules\ai_rule_table_simple.csv"
    generateFunLocFacts inputFile (localFile @"clingo\facts\rule_table_funcloc.lp") 
    generateEquipmentFacts inputFile (localFile @"clingo\facts\rule_table_equipment.lp") 
         
