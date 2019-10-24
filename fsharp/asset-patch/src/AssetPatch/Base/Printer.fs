// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Printer =

    open System
    open System.Text


    open AssetPatch.Base.Syntax
    open AssetPatch.Base.Acronyms

    type Doc = StringBuilder -> StringBuilder

    let renderDoc (doc : Doc) : string = 
        new StringBuilder ()
            |> doc
            |> fun sb -> sb.ToString ()


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

    let punctuate (sep : Doc) (docs : Doc list) : Doc = 
        match docs with
        | [] -> id
        | d1 :: rest -> 
            fun sb -> List.fold (fun ac fn -> ac |> sep |> fn) (d1 sb) rest

    let vcat (docs : Doc list) : Doc = 
        punctuate newline docs

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
       
    let descriptiveHeaderRow (entityType : EntityType) 
                             (headers : HeaderRow) : Doc = 
        let decode = decodeAcronym entityType >> Option.defaultValue ""
        let titles = headers.Columns |> List.map  (decode >> writeText)
        writeText "*" >> intersperse "\t" titles >> newline


    let headerRow (headers : HeaderRow) : Doc = 
        let titles = headers.Columns |> List.map  writeText
        writeText "*" >> intersperse "\t" titles >> newline
            

    // DataRow ends with tab
    let dataRow (row : DataRow) : Doc = 
        let cells = row.Cells |> List.map writeText
        intersperse "\t" cells >> tab >> newline

    let dataRows (rows : DataRow list)  : Doc = 
        applyDoc dataRow rows

    let patchHeader (header : PatchHeader) : Doc = 
        patchType header.PatchType
            >> dataModel header.DataModel
            >> entityType header.EntityType
            >> variant header.Variant
            >> user header.User
            >> dateTime header.DateTime

    let patchToString (patch : PatchFile<'T>) : string = 
        let d1 = 
            patchHeader patch.Header
                >> selection patch.Selection
                >> descriptiveHeaderRow patch.Header.EntityType patch.HeaderRow
                >> headerRow patch.HeaderRow
                >> dataRows patch.DataRows
        renderDoc d1

    let writePatch (outpath : string) (patch : PatchFile<'T>) : unit = 
        let text = patchToString patch
        IO.File.WriteAllText(path=outpath, contents=text)

    // ************************************************************************
    // Variant 'receipt'

    let descriptiveHeaderLines (entityType : EntityType) 
                                (headers : HeaderRow) : Doc = 
        let decode = decodeAcronym entityType >> Option.defaultValue ""
        let titles = headers.Columns |> List.map  (decode >> writeText)
        vcat titles

    let writeReceipt (outpath : string) (patch : PatchFile<'T>) : unit = 
        let text = 
            writeLine "# Variant headings"
                >> descriptiveHeaderLines patch.Header.EntityType patch.HeaderRow
                |> renderDoc
        IO.File.WriteAllText(path=outpath, contents=text)



    let writePatchAndMetadata (outpath : string) (patch : PatchFile<'T>) : unit = 
        let name1 = IO.Path.GetFileNameWithoutExtension(outpath) + ".variant.txt"
        let variantPath = IO.Path.Combine(IO.Path.GetDirectoryName(outpath), name1)        
        writeReceipt variantPath patch
        writePatch outpath patch