// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Web

module Base = 
    
    open System
    open System.Text.RegularExpressions


    let isPliCode (code : string) : bool = 
        Regex.IsMatch(input=code, pattern = "^PLI\d{8}$")

    let isAibCode (code : string) : bool = 
        Regex.IsMatch(input=code, pattern = "^[A-Z]{3}\d{8}$")

    /// Splits on Environment.NewLine
    let toLines (source:string) : string list = 
        source.Split(separator=[| Environment.NewLine |], options=StringSplitOptions.None) 
            |> Array.toList

    let decodeAibCodes (multilineText : string) : string list = 
        let trim (s : string) : string = s.Trim()
        multilineText |> toLines |> List.map trim |> List.filter isAibCode
        

