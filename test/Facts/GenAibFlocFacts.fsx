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


#load "..\..\src\AssetTrafo\Base\FactsCommon.fs"
#load "..\..\src\AssetTrafo\XsbFacts\AibFlocFacts.fs"
open AssetTrafo.XsbFacts.AibFlocFacts


let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\xsb\facts", relativePath)


let main () = 
    let source = @"G:\work\Projects\asset_sync\rules\aib_asset_extract3.csv"
    let rows = getRows source |> Seq.toList
    generateInstallationFacts rows (outputFile "aib_floc_installation.pl")
    generateProcessGroupFacts rows (outputFile "aib_floc_process_group.pl")
    generateProcessFacts rows (outputFile "aib_floc_process.pl")
    generatePlantFacts rows (outputFile "aib_floc_plant.pl")
    generatePlantItemFacts rows (outputFile "aib_floc_plant_item.pl")
    generateCategoryFacts rows (outputFile "aib_asset_category.pl")
    printfn "Done."
