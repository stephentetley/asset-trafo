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
#load "..\..\src\AssetTrafo\XsbFacts\Level234FlocMapping.fs"
open AssetTrafo.XsbFacts.Level234FlocMapping



let xsbOutput (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\xsb\facts", relativePath)


let main () = 
    let source = @"G:\work\Projects\asset_sync\AI2_FLOC_Asset_Hierarchy_Rules_V3_FRAGMENT.xlsx"
    let rows = getMappingRows source
    generateLevel234Mapping rows (xsbOutput "floc_rule_mapping_2_3_4.pl")
    generateDescriptionLookupFacts rows (xsbOutput "s4_description_lookup.pl") 
    // generateProcProcGroupFacts rows (xsbOutput "proc_proc_group.lp") 
