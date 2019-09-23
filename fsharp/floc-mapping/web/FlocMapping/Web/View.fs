// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Web

open Giraffe.GiraffeViewEngine


module View = 

    let makePake (title : string) (content : XmlNode list) = 
        html [] [
            head [] [
                link [ _rel "stylesheet"
                       _type "text/css"
                       _href "/style.css"
                    ]
            ]
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

    let resultsPage (sai : string) (s4Paths : string list): XmlNode =
        
        let p1 = p [] [ str sai ]
        let paths = 
            List.map (fun x -> p [] [str x]) s4Paths
        let plast = 
            p [] [
                a [ _href "/" ] [ str "Back" ]
            ]

        ([p1] @ paths @ [plast]) |> makePake "Aib Code"