// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base


module Common = 
    
    open System
    open FSharp.Core


    type ErrMsg = string    

    let safeName (input:string) : string =
        let replace (problems : char list) (subst : string) (s : string) : string = 
            List.fold (fun (s:string) (c:char) -> s.Replace(c.ToString(), subst)) s problems
        let parens = ['('; ')'; '['; ']'; '{'; '}']
        let bads = ['\\'; '/'; ':'; '?'; '*'] 
        let white = ['\n'; '\t']
        input 
            |> replace parens ""
            |> replace bads "_"
            |> replace white "_"
            |> fun x -> x.Trim() 


    let showS4Date (date : DateTime) : string = 
        date.ToString(format = "dd.MM.yyyy")

    let readS4Date (source : string) : DateTime option = 
        try 
            DateTime.ParseExact(s = source, format = "dd.MM.yyyy", provider = Globalization.CultureInfo.InvariantCulture) 
                |> Some
        with
        |_ -> None
        
           

        
