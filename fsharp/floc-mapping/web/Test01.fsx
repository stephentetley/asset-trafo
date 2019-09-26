// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"

#I @"C:\Users\stephen\.nuget\packages\giraffe\3.6.0\lib\netstandard2.0"
#r "Giraffe.dll"
open Giraffe.GiraffeViewEngine

#load "FlocMapping\Web\Base.fs"
open FlocMapping.Web.Base


let myTemplate name = 
    html [] [
        body [] [
            div [] [
                str (sprintf "Hello %s!" name)
            ]
        ]
    ]
    |> renderHtmlDocument

let demo01 () = 
    myTemplate "world"

let demo02 () = 
    isPliCode "PLI00582858" |> printfn "%b"
    isPliCode "SAI00130367" |> printfn "%b"

let demo03 () = 
    isAibCode "PLI00582858" |> printfn "%b"
    isAibCode "SAI00130367" |> printfn "%b"