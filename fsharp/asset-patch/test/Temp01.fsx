#r "netstandard"
#r "System.Text.Encoding.dll"
open System.Text.RegularExpressions

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


open FSharp.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"

#load "..\src\AssetPatch\Base\Addendum.fs"
#load "..\src\AssetPatch\Base\AssocList.fs"
#load "..\src\AssetPatch\Base\Common.fs"
#load "..\src\AssetPatch\Base\CompilerMonad.fs"
#load "..\src\AssetPatch\Base\ChangeFile.fs"
#load "..\src\AssetPatch\Base\Acronyms.fs"
#load "..\src\AssetPatch\Base\AbsChangeFile.fs"
#load "..\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\Base\EntityTypes.fs"
#load "..\src\AssetPatch\FlocPatch\S4Class.fs"
open AssetPatch.Base
open AssetPatch.Base.ChangeFile
open AssetPatch.Base.CompilerMonad
open AssetPatch.Base.EntityTypes
open AssetPatch.FlocPatch.S4Class

let endsInLoop (s : string) : bool = 
    Regex.IsMatch(input = s, pattern = "Loop$")


/// Apply a rewrite to ``TXTMI - Description (medium text)``
let temp01 () = 
    let source = @"G:\work\Projects\assets\asset_patch\file_download_edm\equi_hydroranger_200.txt"
    
    execCompiler () () <| 
        compile {
            let! rows = 
                readEquiChangeFile source 
                    |>> List.filter (fun x -> not (endsInLoop x.Description))
            do! forMz rows (fun row -> printfn "%s" (row.Description); mreturn())
            return ()
        }




type IS4Class = 
    abstract ClassName : string
    abstract ClInt : uint32

type EAST_NORTH  =
    { Easting : int
      Northing : int 
    }
    interface IS4Class with
        member x.ClassName = "EAST_NORTH"
        member x.ClInt  = 379u


