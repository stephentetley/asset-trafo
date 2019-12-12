﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module ChangeFile =

    open System
    open AssetPatch.Base
   

    [<Struct>]
    type IntegerString = 
        | IntegerString of string

        member x.Number 
            with get () : string = 
                let (IntegerString str) = x in str

        static member OfString (s : string) : IntegerString = 
            IntegerString(s)

        static member Create (width: int, num : uint32) : IntegerString = 
            let s = num.ToString().PadLeft(width, '0')
            IntegerString(s)

    
    type FileType = 
        | Download | Upload

    type DataModel = 
        | U1

    type EntityType = 
        | FuncLoc | ClassFloc | ValuaFloc
        | Equi | ClassEqui | ValuaEqui

    [<Struct>]
    type Selection = 
        | SelectionLine of String
        
        member x.Line 
            with get() : string = let (SelectionLine s) = x in s

    [<Struct>]
    type HeaderRow = 
        | HeaderRow of columns : string []

        member x.Columns 
            with get () : string list = 
                let (HeaderRow arr) = x in Array.toList arr

        member x.GetItem 
            with get (index:int) : string = 
                let (HeaderRow arr) = x in arr.[index]
        
        member x.FieldIndex 
            with get (name: string) : int = 
                let (HeaderRow arr) = x in 
                Array.findIndex (fun x -> x = name) arr

        member x.Select (indices : int list) : HeaderRow = 
            let (HeaderRow arr ) = x 
            List.map (fun ix -> arr.[ix]) indices 
                |> List.toArray
                |> HeaderRow

    
    type Value = string

    [<Struct>]
    type DataRow = 
        | DataRow of Value []        

        static member FromAssocList (assocs : AssocList<string,string>) : DataRow = 
            assocs.Assocs 
                |> List.map snd
                |> List.toArray
                |> DataRow


        member x.Cells 
            with get () : string list = 
                let (DataRow arr ) = x in Array.toList arr
        
        // Calling this Property `GetItem` does not allow index notation, 
        // but we are seeing a "Parameter count mismatch" bug if we call 
        // the property `Item`.
        // FSharp error #5713
        member x.GetItem 
            with get (index:int) : string = 
                match x with
                | DataRow (arr:string[]) -> arr.[index]



        member x.Select (indices : int list) : DataRow = 
            let (DataRow arr ) = x 
            List.map (fun ix -> arr.[ix]) indices 
                |> List.toArray
                |> DataRow

    // Can't really use a Map as we want to preserve order...            
    let internal makeRowRecord (headers : HeaderRow) 
                                (row : DataRow) : AssocList<string, string> = 
        List.foldBack2 (fun a b st -> AssocList.Cons(a,b,st)) headers.Columns row.Cells AssocList.empty


    /// Note Patch header is "common information" it does not
    /// strictly correspond to the commented out meta-data in a patch file
    type FileHeader = 
        { FileType : FileType 
          DataModel : DataModel
          EntityType : EntityType
          Variant : string
          User : string
          DateTime : DateTime
        } 


    type ChangeFile = 
        { Header : FileHeader
          Selection : (Selection list) option
          HeaderDescriptions : HeaderRow option
          HeaderRow : HeaderRow
          DataRows : DataRow list
        }

        /// Column Headings should be the last row of the HeaderRows
        /// (before this we may have a Header Row of descriptions)
        member x.ColumnHeaders 
            with get () : string list = 
                x.HeaderRow.Columns

        member x.ColumnIndex 
            with get (name: string) : int = 
                x.HeaderRow.FieldIndex name

        member x.Indices 
            with get (columns : string list) : int list = 
                List.map (fun name -> x.ColumnIndex(name)) columns

        /// Return the rows as AssocLists pairing column name with value
        /// (This is a thunk rather than a property as it may take some computing...)
        member x.RowAssocs () : AssocList<string,string> list = 
            x.DataRows |> List.map (makeRowRecord x.HeaderRow)


        /// Returns the first assoc row where the predicate matches
        member x.TryFindAssoc (projection : string -> string -> bool) : AssocList<string,string> option = 
            x.RowAssocs () |> List.tryFind (AssocList.tryFindKey projection >> Option.isSome)


    