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


#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat"

#I @"C:\Users\stephen\.nuget\packages\factx\1.0.0-alpha-20190721\lib\netstandard2.0"
#r "FactX"


#load "..\..\src\AssetTrafo\Base\FactsCommon.fs"
#load "..\..\src\AssetTrafo\XsbFacts\Level12FlocMapping.fs"
open AssetTrafo.XsbFacts.Level12FlocMapping


let xsbOutput (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\xsb\facts", relativePath)


let swiOutput (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\output\swi-prolog", relativePath)


let main () = 
    let source = @"G:\work\Projects\asset_sync\Lvl1_2FlocMapping.xlsx"
    let rows = getSiteMappingRows source
    xsbGenerateLevel12Mappings rows (xsbOutput "floc_rule_mapping_1_2.pl") 
    swiGenerateLevel12Mappings rows "floc_rule_mapping_1_2" (swiOutput "floc_rule_mapping_1_2.pl") 
    

