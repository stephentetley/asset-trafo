// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Syntax =

    open System

   

    [<Struct>]
    type IntegerString = 
        | IntegerString of string

        member x.Number 
            with get () : string = 
                let (IntegerString str) = x in str

        static member Create (num : uint32) : IntegerString = 
            IntegerString(num.ToString())

    
    type PatchType = 
        | Download

    type DataModel = 
        | U1

    type EntityType = 
        | FuncLoc | ClassFloc | ValuaFloc
        | Equi | ClassEqui | ValuaEqui

    type SelectionId = 
        | EquiEq of IntegerString 
        | FuncLocEq of string
    
    type HeaderRow = 
        | HeaderRow of columns : string []

        member x.Columns 
            with get () : string list = 
                let (HeaderRow xs ) = x in Array.toList xs
    
    type Value = string

    [<Struct>]
    type DataRow = 
        | DataRow of Value []

        member x.Cells 
            with get () : string list = 
                let (DataRow xs ) = x in Array.toList xs

    type Patch = 
        { PatchType : PatchType 
          DataModel : DataModel
          EntityType : EntityType
          Variant : unit
          User : string
          DateTime : DateTime
          Selection : SelectionId list
          HeaderRow : HeaderRow
          DataRows : DataRow list
        }

        /// Column Headings should be the last row of the HeaderRows
        /// (before this we may have a Header Row of descriptions)
        member x.ColumnHeaders 
            with get () : string list = 
                x.HeaderRow.Columns
