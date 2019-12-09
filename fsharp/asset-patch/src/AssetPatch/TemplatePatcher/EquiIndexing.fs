// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher

// This module is a hack because we don't know Equipment number (EQUI)
// until we have uploaded the Equi file.

module EquiIndexing =
    
    open System.IO

    open FSharp.Interop.Excel
    
    open SheetDoc.Internal.Syntax       
    open SheetDoc.Internal.Render
    open SheetDoc.SheetDoc
    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.PatchTypes

    type RowDoc = SheetDoc.Internal.Syntax.RowDoc


    type IndexingRow =
        { Floc : FuncLocPath
          Description : string
          APIdent : string
          S4Ident : unit
        }

    // ************************************************************************
    // Write spreadsheet


    let private makeOutput (rows : seq<IndexingRow>) : SpreadSheetDoc = 
        let headers : RowDoc = 
            row [ text "Functional Location"
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

    let getIndexingRow (equi : PatchEqui) : IndexingRow = 
        { Floc = equi.FuncLoc
          Description = equi.Description
          APIdent = equi.EquipmentNumber
          S4Ident = ()
        }



    let writeEquiIndexingSheet (outputPath : string)
                                    (rows : PatchEqui list) : CompilerMonad<unit> =
        compile {        
            let doc = rows |> List.map getIndexingRow |> makeOutput
            do! liftAction (fun () -> renderSpreadSheetDoc doc outputPath)
            return ()
        }

    // ************************************************************************
    // Read spreadsheet

    [<Literal>]
    let PROVIDERSOURCE = __SOURCE_DIRECTORY__ + @"\..\..\exceldata\EquiIndexing.xlsx"

    type EquiIndexingTable = 
        ExcelFile< FileName = PROVIDERSOURCE,
                       SheetName = "Equipment Id",
                       ForceString = true >

    type EquiIndexingRow = EquiIndexingTable.Row

   
    let private isBlank (s : string) : bool = 
        match s with
        | null | "" -> true
        | _ -> false


    let readEquiIndexingSheet (xlsxPath : string) : CompilerMonad<(string* string) list> =
        let action () = 
            let source = (new EquiIndexingTable(filename = xlsxPath)).Data
            source
                |> Seq.choose (fun (x : EquiIndexingRow) -> 
                                if isBlank x.``Functional Location`` then 
                                    None 
                                else Some (x.``Asset Patch Id``, x.``S4 Id``))
                |> Seq.toList
        liftAction action

    let materializeEquiFile (lookups : (string * string) list) (patchPath : string) : CompilerMonad<unit> = 
        let action () = 
            let outputFile = Path.ChangeExtension(patchPath, "txt")
            let input = File.ReadAllText(patchPath)
            let text = List.fold (fun (src : string) (target : string, change) -> src.Replace(target, change)) input lookups
            File.WriteAllText(path = outputFile, contents = text)
        liftAction action