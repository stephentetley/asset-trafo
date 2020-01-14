// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Lib


module Common = 

    open System

    // ************************************************************************
    // Read cell functions

    let notBlank (s : string) : bool = 
        match s with
        | null | "" -> false
        | _ -> true


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

    let tryGetNonBlank (source : string) : string option = 
        if String.IsNullOrWhiteSpace source then None else Some source


    /// Note input string might have hh:mm:ss suffix. 
    /// So take first 10 characters.
    let tryGetUSDate (source : string) : DateTime option =
        try
            match DateTime.TryParseExact( s = source.Substring(startIndex=0, length=10)
                                        , format = "MM/dd/yyyy"
                                        , provider = Globalization.CultureInfo.InvariantCulture
                                        , style = Globalization.DateTimeStyles.None) with
            | true, date -> Some date
            | false, _ -> None
        with
        | _ -> None