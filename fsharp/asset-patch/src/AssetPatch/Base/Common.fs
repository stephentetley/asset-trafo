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


    let allSome (source : option<'a> list) : option<'a list> = 
        let rec work xs fk sk = 
            match xs with
            | [] -> sk []
            | Some x :: rest -> 
                work rest fk (fun vs -> 
                sk (x :: vs))
            | None :: _ -> 
                fk ()
        work source (fun _ -> None) (fun xs -> Some xs)


    let allOk (source : Result<'a, 'err> list) : Result<'a list, 'err> = 
        let rec work xs fk sk = 
            match xs with
            | [] -> sk []
            | Ok x :: rest -> 
                work rest fk (fun vs -> 
                sk (x :: vs))
            | Error err :: _ -> 
                fk err
        work source (fun err -> Error err) (fun xs -> Ok xs)

        
        
