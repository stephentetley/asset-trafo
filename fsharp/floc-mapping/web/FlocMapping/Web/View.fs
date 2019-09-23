// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Web

open Giraffe.GiraffeViewEngine


module View = 

    let makePake (title : string) (content : XmlNode list) = 
        html [] [
            head [] []
            body [] [
                main [] content
            ]
        ]

    let saiInputPage : XmlNode =
        [
            form [ _action "/results"; _method "POST" ] [
                div [] [
                    label [] [ str "Aib  Reference:" ]
                    input [ _name "SaiCode"; _type "text" ]
                ]
                input [ _type "submit" ]
            ]
        ] |> makePake "SAI Code"

    let resultsPage (sai : string) : XmlNode =
        [
            p [] [
                str sai
            ]
            
            p [] [
                a [ _href "/" ] [ str "Home" ]
            ]
        ] |> makePake "Aib Code"