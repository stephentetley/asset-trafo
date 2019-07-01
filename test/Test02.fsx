// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

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


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.1.1\lib\netstandard2.0"
#r "FSharp.Data.dll"
#r "FSharp.Data.DesignTime.dll"
open FSharp.Data
open FSharp.Data.JsonExtensions

#load "..\src\AssetTrafo\Base\JsonReader.fs"
#load "..\src\AssetTrafo\Base\Attributes.fs"
#load "..\src\AssetTrafo\Aib\StructureRelationships.fs"
open AssetTrafo.Base.Attributes
open AssetTrafo.Aib.StructureRelationships


let localFile (pathSuffix : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", pathSuffix)



let demo01 () = 
    loadStructure (localFile @"data\ald_toplevel.csv") 
                  (localFile @"data\ald_structure_relationships.csv")
