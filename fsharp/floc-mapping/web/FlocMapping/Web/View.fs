// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Web

open Giraffe.GiraffeViewEngine


module View = 

    let makePage (pageTitle : string) (content : XmlNode list) = 
        html [] [
            head [] [
                link [ _rel "stylesheet"
                       _type "text/css"
                       _href "/style.css"
                    ]
                title [] [ str pageTitle ]
            ]
            body [] [
                main [] content
            ]
        ]


    // TODO - this page could list recent sainums
    let saiInputPage : XmlNode =
        [
            form [ _action "/results"; _method "POST" ] [
                div [] [
                    label [] [ str "Aib  Reference:" ]
                    input [ _name "SaiCode"; _type "text" ]
                ]
                input [ _type "submit" ]
            ]
        ] |> makePage "AI2 SAI / PLI Code"

    let resultsPage (sai : string) 
                    (commonName : string) 
                    (s4Paths : (string * string) list): XmlNode =
        [
            yield p [] [ str sai ]
            
            yield p [] [ span [_id "commonname"] [ str commonName ] ]

            yield p [] [ str "S4 Flocs:" ]

            yield p [] [ 
                table [] 
                    (List.map (fun (x,y) -> 
                        tr [] [
                                td [] [str x] 
                                td [_id "commonname"] [str y]
                              ]) s4Paths)
                ]

            yield p [] [
                a [ _href "/" ] [ str "Back" ]
            ]
        ] |> makePage "S4 Mappings"