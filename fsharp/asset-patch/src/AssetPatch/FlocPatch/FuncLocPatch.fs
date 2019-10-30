// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocPatch



module FuncLocPatch =
    
    
    open FSharp.Core

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.EntityTypes
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.FlocPatch.Common    




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
        ; "INBDT"           // Start-up date
        ; "STATTEXT"        // Status
        ; "USTW_FLOC"       // Status of an object
        ; "TPLKZ_FLC"       // Structure indicator
        ; "TPLMA1"          // Superior FL for CR Processing
        ; "TPLMA"           // Superior FunctLoc
        ; "LGWIDI"          // Work center origin
        ]

    let private funcLocSetAttrs (funcLoc : FuncLoc) : CompilerMonad<FuncLoc, 'env, 'acc>  = 
        let update flocParent = fun attrs ->
            attrs
                |> AssocList.update "STATEXT" "UCON"
                |> AssocList.update "USTAFLOC" "UCON"
                |> AssocList.update "USTW_FLOC" "UCON"                    
                |> AssocList.update "FUNCLOC" (funcLoc.Path.ToString())
                |> AssocList.update "TXTMI" funcLoc.Description
                |> AssocList.update "FLTYP" (funcLoc.Level.ToString())
                |> AssocList.update "IEQUI" (if funcLoc.Level >= 5 then "X" else "")
                |> AssocList.update "EQART" funcLoc.ObjectType
                |> AssocList.update "TPLMA1" (flocParent.ToString())
                |> AssocList.update "TPLMA" (flocParent.ToString())
                |> AssocList.select selectList
        compile {
            let! flocParent = liftOption <| parent funcLoc.Path
            return { funcLoc with Attributes = update flocParent funcLoc.Attributes }
        }



    let makeFuncLocPatch (user : string) 
                        (timestamp : System.DateTime)
                        (funcLocs : FuncLoc list) : CompilerMonad<ChangeFile, 'env, 'acc> = 
        compile {
            let! rows = funcLocs |> mapM funcLocSetAttrs 
            return! compileFuncLocFile user timestamp rows
        }

