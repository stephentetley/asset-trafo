// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module InputData =
    
    open System
    open FSharp.Interop.Excel

    open AssetPatch.TemplatePatcher.CompilerMonad

    [<Literal>]
    let PROVIDERSOURCE = __SOURCE_DIRECTORY__ + @"\..\excel-sample\WorkListSample.xlsx"

    type WorkListTable = 
        ExcelFile< FileName = PROVIDERSOURCE,
                       SheetName = "Work_List",
                       ForceString = true >

    type WorkListRow = WorkListTable.Row

    let private notBlank (s : string) : bool = 
        match s with
        | null | "" -> false
        | _ -> true


    let readWorkList (xlsxPath : string) : CompilerMonad<WorkListRow list> =
        let action () = 
            let source = (new WorkListTable(filename = xlsxPath)).Data
            source
                |> Seq.choose (fun (row : WorkListRow) -> 
                                if notBlank row.``S4 Root FuncLoc`` then Some row else None)
                |> Seq.toList
        liftAction action


    // ************************************************************************
    // Read cell functions

    let tryGetInt (source : string) : int option = 
        try
            int source |> Some
        with
        | _ -> None

    let tryGetDecimal (source : string) : decimal option = 
        try
            decimal source |> Some
        with
        | _ -> None

    let tryGetString (source : string) : string option = 
        match source with
        | null -> None
        | _ -> Some source

    /// Note input string might have hh:mm:ss suffix. 
    /// So take first 10 characters.
    let tryGetUSDate (source : string) : DateTime option =
        printfn "tryGetDate: %s" source
        match DateTime.TryParseExact( s = source.Substring(startIndex=0, length=10)
                                    , format = "MM/dd/yyyy"
                                    , provider = Globalization.CultureInfo.InvariantCulture
                                    , style = Globalization.DateTimeStyles.None) with
        | true, date -> Some date
        | false, _ -> None
