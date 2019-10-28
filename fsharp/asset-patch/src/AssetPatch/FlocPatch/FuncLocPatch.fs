// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocPatch



module FuncLocPatch =
    
    
    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.FlocPatch.Common


    type private Env = Unit
    type FLCompiler<'a> = CompilerMonad<'a, Env>

    let runFLCompiler (action : FLCompiler<'a>) = 
        runCompiler () action

    let excludeList : string list = 
        // Field x comment
        [ "JOBJN_FL"        // Magic Number
        ; "FLOC_REF"        // Magic number
        ]

    let selectList : string list = 
        // Field x comment
        [ "FUNCLOC"         // Functional Location
        ; "TXTMI"           // Description (medium text) 
        ; "BUKRSI"          // CompCode origin #inherit
        ; "KOKRSI"          // ControlArea origin #inherit
        ; "USTA_FLOC"       // Display lines for user status
        ; "FLTYP"           // FuncLocCategory
        ; "IEQUI"           // Installation allowed
        ; "SWERKI"          // MaintPlant origin
        ; "OBJTYFLOC"       // Object Type
        ; "EQART"           // Object Type        
        ; "PPSIDI"          // PP WrkCenter origin
        ; "BEBERI"          // Plant Section Origin               
        ; "STATTEXT"        // Status
        ; "USTW_FLOC"       // Status of an object
        ; "TPLKZ_FLC"       // Structure indicator
        ; "TPLMA1"          // Superior FL for CR Processing
        ; "TPLMA"           // Superior FunctLoc
        ; "LGWIDI"          // Work center origin
        ]

    let private funcLocAssocList (funcLoc : FuncLoc) : FLCompiler<AssocList<string,string>> = 
        compile {
            
            let! parent = liftOption funcLoc.FuncLocPath.Parent
            return 
                funcLoc.InheritedAttributes
                    |> AssocList.update "STATEXT" "UCON"
                    |> AssocList.update "USTAFLOC" "UCON"
                    |> AssocList.update "USTW_FLOC" "UCON"                    
                    |> AssocList.update "FUNCLOC" (funcLoc.FuncLocPath.ToString())
                    |> AssocList.update "TXTMI" funcLoc.Description
                    |> AssocList.update "FLTYP" (funcLoc.Level.ToString())
                    |> AssocList.update "IEQUI" (if funcLoc.Level >= 5 then "X" else "")
                    |> AssocList.update "EQART" funcLoc.ObjectType
                    |> AssocList.update "TPLMA1" (parent.ToString())
                    |> AssocList.update "TPLMA" (parent.ToString())
                    |> AssocList.select selectList
        }



    let makeFuncLocPatch (user : string) 
                        (timestamp : System.DateTime)
                        (funcLocs : FuncLoc list) : FLCompiler<ChangeFile> = 
        compile {
            let! rows = 
                funcLocs |> List.sort |> mapM funcLocAssocList 
            return! makeChangeFile FuncLoc user timestamp rows
        }

