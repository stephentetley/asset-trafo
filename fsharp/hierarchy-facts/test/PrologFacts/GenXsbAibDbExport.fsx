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


#load "..\..\src\AssetTrafo\Base\FactsCommon.fs"
#load "..\..\src\AssetTrafo\Base\DbExportSchema.fs"
#load "..\..\src\AssetTrafo\PrologFacts\XsbAibDbExport.fs"
open AssetTrafo.Base.DbExportSchema
open AssetTrafo.PrologFacts.XsbAibDbExport


let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\..\xsb\facts", relativePath)


let genFlocFacts (csvSource : string) : unit = 
    let rows = getFlocRows csvSource |> Seq.toList
    generateInstallationFacts rows (outputFile "aib_floc_l1_l2_installation.pl")
    generateProcessGroupFacts rows (outputFile "aib_floc_l3_process_group.pl")
    generateProcessFacts rows (outputFile "aib_floc_l4_process.pl")
    generatePlantFacts rows (outputFile "aib_floc_l5_plant.pl")
    generatePlantItemFacts rows (outputFile "aib_floc_l6_plant_item.pl")
    generateCategoryFacts rows (outputFile "aib_asset_category.pl")

let genEquipmentFacts (csvSource : string) : unit =  
    let rows = getEquipmentRows csvSource |> Seq.toList
    generateEquipmentFacts rows (outputFile "aib_equipment.pl")

let main () = 
    genFlocFacts @"G:\work\Projects\asset_sync\rules\aib_floc_extract4.csv"
    genEquipmentFacts @"G:\work\Projects\asset_sync\rules\aib_equipment_extract1.csv"
    printfn "Done." 
