// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module Addendum =
    
    open SLSqlite.Core
    open MarkdownDoc.Markdown


    // Potential additions to library with unstable APIs 
    // (sl-sqlite, markdown-doc, ...)

    // ************************************************************************
    // For sl-sqlite

    let valueByName (reader : RowReader) (field : string) : obj = 
        let ix = reader.GetOrdinal(field)
        reader.GetValue(ix)

    let getString (reader : RowReader) (field : string) : string = 
        let ix = reader.GetOrdinal(field)
        reader.GetString(ix)


    // ************************************************************************
    // For MarkdownDoc

    /// Add to markdown-doc?
    let commaSpaceSep (texts : Text list) : Text = 
        textPunctuate (rawtext ", ") texts

    type ColumnHeading = 
        { ColumnName : Markdown
          ColumnFormatting : ColumnSpec 
        }

    type ColumnHeadings = ColumnHeading list

    let alignDefault (width : int) (columnName : Markdown) : ColumnHeading = 
        let spec = { ColumnSpec.Width = width
                   ; ColumnSpec.Alignment = Alignment.AlignDefault }
        { ColumnName = columnName; ColumnFormatting = spec }

    let alignLeft (width : int) (columnName : Markdown) : ColumnHeading = 
        let spec = { ColumnSpec.Width = width
                   ; ColumnSpec.Alignment = Alignment.AlignLeft }
        { ColumnName = columnName; ColumnFormatting = spec }


    let alignCenter (width : int) (columnName : Markdown) : ColumnHeading = 
        let spec = { ColumnSpec.Width = width
                   ; ColumnSpec.Alignment = Alignment.AlignCenter }
        { ColumnName = columnName; ColumnFormatting = spec }

    let alignRight (width : int) (columnName : Markdown) : ColumnHeading = 
        let spec = { ColumnSpec.Width = width
                   ; ColumnSpec.Alignment = Alignment.AlignRight }
        { ColumnName = columnName; ColumnFormatting = spec }



    let makeTableWithHeadings (headings : ColumnHeadings) 
                              (rows : TableRow list) : Table = 
        let colSpecs = headings |> List.map (fun x -> x.ColumnFormatting)
        let colNames = headings |> List.map (fun x -> x.ColumnName)
        makeTable colSpecs colNames rows
