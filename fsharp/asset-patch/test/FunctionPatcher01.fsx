#r "netstandard"
#r "System.Text.Encoding.dll"
#r "System.Xml.Linq"
#r "System.Xml.ReaderWriter"
#r "System.Xml.XDocument"
#r "System.IO.FileSystem.Primitives"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\lib\netstandard2.0"
#r "ExcelProvider.Runtime.dll"

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\typeproviders\fsharp41\netstandard2.0"
#r "ExcelDataReader.DataSet.dll"
#r "ExcelDataReader.dll"
#r "ExcelProvider.DesignTime.dll"

#I @"C:\Users\stephen\.nuget\packages\system.io.packaging\4.5.0\lib\netstandard1.3"
#r "System.IO.Packaging"
#I @"C:\Users\stephen\.nuget\packages\DocumentFormat.OpenXml\2.9.1\lib\netstandard1.3"
#r "DocumentFormat.OpenXml"


#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


open FSharp.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"

#I @"C:\Users\stephen\.nuget\packages\sheetdoc\1.0.0-alpha-20191121a\lib\netstandard2.0"
#r "SheetDoc.dll"


#load "..\src\AssetPatch\Base\Addendum.fs"
#load "..\src\AssetPatch\Base\Common.fs"
#load "..\src\AssetPatch\Base\AssocList.fs"
#load "..\src\AssetPatch\Base\CompilerMonad.fs"
#load "..\src\AssetPatch\Base\ChangeFile.fs"
#load "..\src\AssetPatch\Base\Acronyms.fs"
#load "..\src\AssetPatch\Base\AbsChangeFile.fs"
#load "..\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\src\AssetPatch\Base\Parser.fs"
#load "..\src\AssetPatch\Base\Printer.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchTypes.fs"
#load "..\src\AssetPatch\TemplatePatcher\Hierarchy.fs"
#load "..\src\AssetPatch\TemplatePatcher\Template.fs"
#load "..\src\AssetPatch\TemplatePatcher\EquiIndexing.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchWriter.fs"
#load "..\src\AssetPatch\TemplatePatcher\EmitEquipment.fs"
#load "..\src\AssetPatch\TemplatePatcher\EmitFuncLoc.fs"
#load "..\src\AssetPatch\TemplatePatcher\Emitter.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchCompiler.fs"
#load "..\src\AssetPatch\TemplateCatalogue\Base.fs"
open AssetPatch.Base.CompilerMonad
open AssetPatch.Base.FuncLocPath
open AssetPatch.TemplatePatcher.Template
open AssetPatch.TemplatePatcher.PatchCompiler
open AssetPatch.TemplateCatalogue


let outputDirectory (child : string) : string = 
    match child with 
    | null | "" -> Path.Combine(__SOURCE_DIRECTORY__, @"..\output")
    | _ -> Path.Combine(__SOURCE_DIRECTORY__, @"..\output", child)


type RowParams = 
    { Code : string
      Name : string 
      Easting : int
      Northing : int
      EquiFlocSaiNumber : string option
      EquiPliNumber : string option
    }


  


let edcTemplate (parameters : RowParams) : Function = 
    let east_north_common = 
        east_north [ easting parameters.Easting; northing parameters.Northing ]
    
    environmental_discharge 
        [ east_north_common 
          aib_reference [ s4_aib_reference () ] 
        ]
        [ 
          liquid_discharge
            [ east_north_common
              aib_reference [ s4_aib_reference () ] 
            ]
            [   
              regulatory_monitoring
                [ east_north_common 
                  aib_reference [ s4_aib_reference () ]    
                ]
                [   
                  montoring_system "SYS01" "EA Event Duration Monitoring"
                    [ east_north_common 
                      aib_reference [ s4_aib_reference () ]
                    ]
                    _no_assemblies_
                    [ 
                      lstn_level_transmitter "Storm Overflow Level Monitor Loop"
                        [ east_north_common
                          aib_reference [ s4_aib_reference () ]
                        ]
                        _no_subordinate_equipment_
                        [ manufacturer "SIEMENS"
                          model "HYDRORANGER 200" 
                        ]
                    ]
                ]
            ]
        ]



let test01 () = 
    let worklist = 
        [ ("KRI03", {Code = "KRI03"; Name = "Kriddle SPS"; Easting = 492729; Northing = 477323; EquiFlocSaiNumber = Some "SAI00043252"; EquiPliNumber = Some "PLI00002001" } )
        ] 
        |> List.map (fun (name, v) -> (FuncLocPath.Create name, v))
    runCompiler (defaultEnv "TETLEYS") 
       <| compileFunctionPatches 
                   (outputDirectory "edg-patches")
                   "env_discharge"
                   edcTemplate
                   worklist

// This template has optional elements that are possible but a bit ugly...
// We have to represent option elements as a list that might have zero-or-one
// elements then `yield!` them.
// Single (non-optional) elements need to use `yield` when in the same list.
// The symbolic combinators (&&=) and (??=) are largely superfluous.
// 
let caaTemplate (parameters : RowParams) : Site = 
    let east_north_common = 
        east_north [ easting parameters.Easting; northing parameters.Northing ]
    
    _site parameters.Code parameters.Name
        [ east_north_common 
          aib_reference [ s4_aib_reference () ] 
        ]
        [
            control_automation 
                [ east_north_common 
                  aib_reference [ s4_aib_reference () ] 
                ]
                [ 
                  networks
                    [ east_north_common
                      aib_reference [ s4_aib_reference () ] 
                    ]
                    [   
                      telemetry
                        [ east_north_common 
                          aib_reference [ s4_aib_reference () ]    
                        ]
                        [   
                          telemetry_system "SYS01" "Telemetry System"
                            [ east_north_common 
                              aib_reference [ s4_aib_reference () ]
                            ]
                            _no_assemblies_
                            [ 
                              telemetry_outstation "Telemetry Outstation"
                                [ east_north_common
                                  aib_reference                             
                                    [ s4_aib_reference ()
                                      applyOptional ai2_aib_reference parameters.EquiFlocSaiNumber
                                      applyOptional ai2_aib_reference parameters.EquiPliNumber
                                    ]
                                ]
                                _no_subordinate_equipment_
                                [ manufacturer "METASPHERE"
                                  model "MMIM" 
                                  serial_number "TO BE DETERMINED"
                                ]
                            ]
                        ]            
                    ]
                ]
            ]

let test02 () = 
    let worklist = 
        [ {Code = "SPT60"; Name = "Stephen SPS"; Easting = 492729; Northing = 477323; EquiFlocSaiNumber = Some "SAI00043252"; EquiPliNumber = Some "PLI00002001"} 
        ] |> List.map (fun r1 -> (FuncLocPath.Create r1.Code, r1))
    runCompiler (defaultEnv "TETLEYS") 
       <| compileSitePatches 
                   (outputDirectory "caa-patches")
                   "control_automation"
                   caaTemplate
                   worklist


let test02b () = 
    runCompiler (defaultEnv "TETLEYS") 
       <| materializeEquiClassValuaPatches (outputDirectory "caa-patches")

