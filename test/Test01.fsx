// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO

#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.1.1\lib\netstandard2.0"
#r "FSharp.Data.dll"
#r "FSharp.Data.DesignTime.dll"
open FSharp.Data

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190712\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\AssetTrafo\Base\CompilerMonad.fs"
#load "..\src\AssetTrafo\Base\JsonReader.fs"
#load "..\src\AssetTrafo\Base\Attributes.fs"
#load "..\src\AssetTrafo\Aib\Syntax.fs"
#load "..\src\AssetTrafo\S4\S4Types.fs"

open AssetTrafo.Base
open AssetTrafo.Aib.Syntax

let localFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", relativePath)

let demo01 () = 
    readAibInstallationJson <| localFile @"output\ald_new_structure.json"



