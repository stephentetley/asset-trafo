// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base


module Common = 

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
