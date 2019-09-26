// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Web


module View = 
    
    open Giraffe.GiraffeViewEngine

    open FlocMapping.Web.Model
    open FlocMapping.Web.Base

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
                    (answers : FlocAnswer list): XmlNode =
        [
            yield p [] [ str sai ]
            
            yield p [] [ span [_id "commonName"] [ str commonName ] ]

            yield p [] [ str "S4 Flocs:" ]

            yield p [] [ htmlTable answers ] 

            yield p [] [
                a [ _href "/" ] [ str "Back" ]
            ]
        ] |> makePage "S4 Mappings"