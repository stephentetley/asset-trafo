// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Printer =

    open System
    open System.Text


    open AssetPatch.Base.Syntax

    type Doc = StringBuilder -> StringBuilder

    let applyDoc (writer : 'a -> Doc) (items : 'a list) : Doc = 
        fun sb -> List.fold (fun ac x -> writer x ac) sb items

    let private tab : Doc = 
        fun sb -> sb.Append "\t"

    let private newline : Doc = 
        fun sb -> sb.AppendLine ""

    let private writeLine (source : string) : Doc = 
        fun sb -> sb.AppendLine source

    let private writeText (source : string) : Doc = 
        fun sb -> sb.Append source
    
    let private directiveLine (source : string) : Doc = 
        "* " + source |> writeLine

    let intersperse (sep : string) (docs : Doc list) : Doc = 
        match docs with
        | [] -> id
        | d1 :: rest -> 
            fun sb -> List.fold (fun ac fn -> ac |> writeText sep |> fn) (d1 sb) rest

    let patchType (source : PatchType) : Doc = 
        directiveLine <|
            match source with
            | Download -> "Download"
            
    let dataModel (source : DataModel) : Doc = 
        let dmname = 
            match source with
            | U1 -> "U1"
        directiveLine <| sprintf "Data Model: %s" dmname 
            
    

    let entityType (source : EntityType) : Doc =
        let etname = 
            match source with
            | FuncLoc -> "FUNCLOC"
            | ClassFloc -> "CLASSFLOC"
            | ValuaFloc -> "VALUAFLOC"
            | Equi -> "EQUI"
            | ClassEqui -> "CLASSEQUI" 
            | ValuaEqui -> "VALUAEQUI"
        directiveLine <| sprintf "Entity Type: %s" etname
            
    let variant () : Doc = 
        directiveLine <| "Variant:"

    let user (userName : string) : Doc = 
        directiveLine <| sprintf "User: %s" userName

    let dateTime (dt : DateTime) : Doc = 
        directiveLine 
            <| sprintf "Date: %s / Time: %s"
                    (dt.ToString(format="yyyyMMdd"))
                    (dt.ToString(format="HHmmss"))


    let selectionId (source : SelectionId) : Doc = 
        directiveLine <| 
            match source with
            | EquiEq num -> sprintf "EQUI EQ | %s |" num.Number
            | FuncLocEq floc -> sprintf "FUNCLOC EQ | %s |" floc

    let selection (items : SelectionId list) : Doc = 
        directiveLine "Selection:" >> applyDoc selectionId items
       
    let headerRow (headers : HeaderRow) : Doc = 
        let titles = headers.Columns |> List.map  writeText
        writeText "*" >> intersperse "\t" titles >> newline
            

    // DataRow ends with tab
    let dataRow (row : DataRow) : Doc = 
        let cells = row.Cells |> List.map writeText
        intersperse "\t" cells >> tab >> newline

    let dataRows (rows : DataRow list)  : Doc = 
        applyDoc dataRow rows


    let patchToString (patch : PatchFile) : string = 
        new StringBuilder ()
            |> patchType patch.PatchType
            |> dataModel patch.DataModel
            |> entityType patch.EntityType
            |> variant patch.Variant
            |> user patch.User
            |> dateTime patch.DateTime
            |> selection patch.Selection
            |> headerRow patch.HeaderRow
            |> dataRows patch.DataRows
            |> fun sb -> sb.ToString ()

    let writePatch (outpath : string) (patch : PatchFile) : unit = 
        let text = patchToString patch
        IO.File.WriteAllText(path=outpath, contents=text)

