#I @"C:\Users\stephen\.nuget\packages\giraffe\3.6.0\lib\netstandard2.0"
#r "Giraffe.dll"

open Giraffe.GiraffeViewEngine

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