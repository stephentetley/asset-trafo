#r "netstandard"
#r "System.Text.Encoding.dll"
open System.IO

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


open FSharp.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"

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
#load "..\src\AssetPatch\TemplatePatcher\Renamer.fs"
#load "..\src\AssetPatch\TemplatePatcher\Emitter.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchGen.fs"
#load "..\src\AssetPatch\TemplatePatcher\PatchCompiler.fs"
#load "..\src\AssetPatch\TemplatePatcher\Catalogue.fs"
open AssetPatch.Base.CompilerMonad
open AssetPatch.Base.FuncLocPath
open AssetPatch.TemplatePatcher.Template
open AssetPatch.TemplatePatcher.PatchCompiler
open AssetPatch.TemplatePatcher.Catalogue


let outputDirectory () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output")

type GeoParams = 
    { Easting : int
      Northing : int
    }


let edgTemplate (parameters : GeoParams) : Function = 
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
                    ]
                ]
            ]
        ]

let test01 () = 
    let worklist = 
        [ ("KRI03", {Easting = 492729; Northing = 477323} )
        ] 
        |> List.map (fun (name, v) -> (FuncLocPath.Create name, v))
    runCompiler (defaultEnv ()) 
       <| compileFunctionPatches 
                   (outputDirectory ())
                   "env_discharge"
                   "TETLEYS" 
                   edgTemplate
                   worklist


let caaTemplate (parameters : GeoParams) : Function = 
    let east_north_common = 
        east_north [ easting parameters.Easting; northing parameters.Northing ]
    
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
                          aib_reference [ s4_aib_reference () ]
                        ]
                        _no_subordinate_equipment_
                    ]
                ]            
            ]
        ]

let test02 () = 
    let worklist = 
        [ ("KRI03", {Easting = 492729; Northing = 477323} )
        ] 
        |> List.map (fun (name, v) -> (FuncLocPath.Create name, v))
    runCompiler (defaultEnv ()) 
       <| compileFunctionPatches 
                   (outputDirectory ())
                   "control_automation"
                   "TETLEYS" 
                   caaTemplate
                   worklist
