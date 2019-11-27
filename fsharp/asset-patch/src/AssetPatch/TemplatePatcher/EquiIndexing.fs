// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module EquiIndexing =
    
    
    open SheetDoc.Internal.Syntax       // shouldn't need to do this...
    open SheetDoc.Internal.Render
    open SheetDoc.SheetDoc

    open AssetPatch.Base.CompilerMonad
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.PatchTypes

    type RowDoc = SheetDoc.Internal.Syntax.RowDoc


    type IndexingRow =
        { Floc : FuncLocPath
          Description : string
          APIdent : string
          S4Ident : unit
        }


    let private makeOutput (rows : seq<IndexingRow>) : SpreadSheetDoc = 
        let headers : RowDoc = 
            row [ text "Functiona Location"
                ; text "Description"
                ; text "Asset Patch Id"
                ; text "S4 Id" ]
        let makeRow (ixRow : IndexingRow) : RowDoc = 
            row [ ixRow.Floc.ToString() |> text
                ; text ixRow.Description
                ; text ixRow.APIdent
                ; blankCell ]
        spreadsheet 
            [ sheet "Equipment Id"
                [ yield headers
                ; yield! Seq.map makeRow rows
                ]                
            ]

    ///// Compile a list for ClassEqui changes into a ChangeFile
    //let compileEquiFile (rows : Equi list) : CompilerMonad<SpreadSheetDoc> = 
    //    failwith "IndexingRow"

    let getIndexingRow (equi : Equi) : IndexingRow = 
        { Floc = equi.FuncLoc
          Description = equi.Description
          APIdent = equi.EquipmentNumber
          S4Ident = ()
        }



    let writeEquiIndexingSheet (outputPath: string)
                                    (rows : Equi list) : CompilerMonad<unit> =
        compile {        
            let doc = rows |> List.map getIndexingRow |> makeOutput
            do! liftAction (fun () -> renderSpreadSheetDoc doc outputPath)
            return ()
        }

