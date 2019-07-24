// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\lib\netstandard2.0"
#r "ExcelProvider.Runtime.dll"

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\typeproviders\fsharp41\netstandard2.0"
#r "ExcelDataReader.DataSet.dll"
#r "ExcelDataReader.dll"
#r "ExcelProvider.DesignTime.dll"
open FSharp.Interop.Excel

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat"

#I @"C:\Users\stephen\.nuget\packages\factx\1.0.0-alpha-20190721\lib\netstandard2.0"
#r "FactX"


#I @"C:\Users\stephen\.nuget\packages\slpotassco\1.0.0-alpha-20190721\lib\netstandard2.0"
#r "SLPotassco"
open SLPotassco.Potassco.Invoke

#load "..\..\src\AssetTrafo\AspFacts\Common.fs"
#load "..\..\src\AssetTrafo\AspFacts\Level12FlocMapping.fs"
open AssetTrafo.AspFacts.Level12FlocMapping

let clingoDirectory () = 
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, @"..\..\clingo")


let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\clingo\facts", relativePath)


let main () = 
    let source = @"G:\work\Projects\asset_sync\Lvl1_2FlocMapping.xlsx"
    let rows = getSiteMappingRows source
    generateLevel12Mappings rows (outputFile "floc_mapping_1_2.lp") 
    generateCommonNameFacts rows (outputFile "aib_common_names.lp") 
    generateAibInstallationType rows (outputFile "aib_installation_type.lp") 

