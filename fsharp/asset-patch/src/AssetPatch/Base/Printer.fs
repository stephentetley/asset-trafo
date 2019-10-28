// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Printer =

    open System

    open AssetPatch.Base.Addendum
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.Acronyms
   

    
    let private comment (source : string) : Doc = 
        character '*' ^+^ text source

    let private defines (name: string) (value : string) : Doc = 
        character '*' ^+^ text name ^^ colon ^+^ text value

    
    // ************************************************************************
    // Print a doc 
    // This is line oriented so we don't use a *pretty* printer 
    // which tries to fit lines.


    let fileType (source : FileType) : Doc = 
        comment <|
            match source with
            | Download -> "Download"
            
    let dataModel (source : DataModel) : Doc = 
        let dmname = 
            match source with
            | U1 -> "U1"
        defines "Data Model" dmname 
            
    

    let entityType (source : EntityType) : Doc =
        let etname = 
            match source with
            | FuncLoc -> "FUNCLOC"
            | ClassFloc -> "CLASSFLOC"
            | ValuaFloc -> "VALUAFLOC"
            | Equi -> "EQUI"
            | ClassEqui -> "CLASSEQUI" 
            | ValuaEqui -> "VALUAEQUI"
        defines "Entity Type" etname
            
    let variant () : Doc = 
        comment "Variant:"

    let user (userName : string) : Doc = 
        defines "User" userName

    let dateTime (dt : DateTime) : Doc = 
        comment
            <| sprintf "Date: %s / Time: %s"
                        (dt.ToString(format="yyyyMMdd"))
                        (dt.ToString(format="HHmmss"))
                         


    let selectionId (source : SelectionId) : Doc = 
        comment <| 
            match source with
            | EquiEq num -> sprintf "EQUI EQ | %s |" num.Number
            | FuncLocEq floc -> sprintf "FUNCLOC EQ | %s |" floc

    let selection (items : SelectionId list) : Doc = 
        comment "Selection:" 
            ^!^ vcat (List.map selectionId items)
       
    let descriptiveHeaderRow (entityType : EntityType) 
                             (headers : HeaderRow) : Doc = 
        let decode = decodeAcronym entityType >> Option.defaultValue ""
        let titles = headers.Columns |> List.map  (decode >> text)
        text "*" ^^ punctuate tab titles


    let headerRow (headers : HeaderRow) : Doc = 
        let titles = headers.Columns |> List.map text
        text "*" ^^ punctuate tab titles
            

    // DataRow ends with tab
    let dataRow (row : DataRow) : Doc = 
        let cells = row.Cells |> List.map text
        punctuate tab cells >> tab

    let dataRows (rows : DataRow list)  : Doc = 
        vcat <| List.map dataRow rows

    let fileHeader (header : FileHeader) : Doc = 
        vcat 
            [ fileType header.PatchType
            ; dataModel header.DataModel
            ; entityType header.EntityType
            ; variant header.Variant
            ; user header.User
            ; dateTime header.DateTime ]

    let changeFileToString (changeFile : ChangeFile<'T>) : string = 
        let d1 = 
            fileHeader changeFile.Header
                ^!^ selection changeFile.Selection
                ^!^ descriptiveHeaderRow changeFile.Header.EntityType changeFile.HeaderRow
                ^!^ headerRow changeFile.HeaderRow
                ^!^ dataRows changeFile.DataRows
                ^!^ empty
        render d1

    let writeChangeFile (outpath : string) 
                        (changeFile : ChangeFile<'T>) : unit = 
        let text = changeFileToString changeFile
        IO.File.WriteAllText(path=outpath, contents=text)

    // ************************************************************************
    // Variant 'receipt'

    let descriptiveHeaderLines (entityType : EntityType) 
                                (headers : HeaderRow) : Doc = 
        let decode = decodeAcronym entityType >> Option.defaultValue ""
        let titles = headers.Columns |> List.map  (decode >> text)
        vcat titles

    let writeReceipt (outpath : string) (changeFile : ChangeFile<'T>) : unit = 
        let text = 
            text "# Variant headings"
                ^!^ descriptiveHeaderLines changeFile.Header.EntityType changeFile.HeaderRow
                |> render
        IO.File.WriteAllText(path=outpath, contents=text)



    let writePatchAndMetadata (outpath : string) 
                              (changeFile : ChangeFile<'T>) : unit = 
        let name1 = IO.Path.GetFileNameWithoutExtension(outpath) + ".variant.txt"
        let variantPath = IO.Path.Combine(IO.Path.GetDirectoryName(outpath), name1)        
        writeReceipt variantPath changeFile
        writeChangeFile outpath changeFile