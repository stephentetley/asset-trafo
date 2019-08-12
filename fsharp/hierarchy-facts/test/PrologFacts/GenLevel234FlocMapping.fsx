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


#load "..\..\src\AssetSync\Base\FactsCommon.fs"
#load "..\..\src\AssetSync\PrologFacts\Level234FlocMapping.fs"
open AssetSync.PrologFacts.Level234FlocMapping



let xsbOutput (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\..\xsb\facts", relativePath)

let swiOutput (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\..\output\swi-prolog", relativePath)

let main () = 
    let source = @"G:\work\Projects\asset_sync\AI2_FLOC_Asset_Hierarchy_Rules_V3_FRAGMENT.xlsx"
    let rows = getMappingRows source
    // Xsb
    xsbLevel23Mapping rows (xsbOutput "floc_rule_mapping_2_3.pl")
    xsbLevel234Mapping rows (xsbOutput "floc_rule_mapping_2_3_4.pl")
    xsbDescriptionLookupFacts rows (xsbOutput "s4_description_lookup.pl")
    // Swi-Prolog
    swiLevel23Mapping "floc_rule_mapping_2_3" rows (swiOutput "floc_rule_mapping_2_3.pl")
    swiLevel234Mapping "floc_rule_mapping_2_3_4" rows (swiOutput "floc_rule_mapping_2_3_4.pl")
    swiDescriptionLookupFacts "s4_description_lookup" rows (swiOutput "s4_description_lookup.pl")  
