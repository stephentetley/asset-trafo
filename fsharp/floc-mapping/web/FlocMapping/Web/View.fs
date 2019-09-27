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
                fieldset [] [
                    p [] [
                        label [_for "SingleReference" ] [ str "Aib Reference:" ]
                        input [ _name "SingleReference"; _type "text" ]
                    ]
                    p [] [
                        label [_for "MultipleReferences" ] [ str "Multiple References:" ]
                        textarea [ _name "MultipleReferences"; _rows "12" ] []
                    ]
                    p [] [ 
                        input [ _type "submit" ]
                    ]
                ]
            ]
        ] |> makePage "AI2 SAI / PLI Code"

    
    
    let resultsPage (sai : string) 
                    (commonName : string) 
                    (answers : LookupAnswer list): XmlNode =
        [
            
            yield p [] [ str "AI2:" ]
            
            yield p [] [ 
                htmlTable [ 
                    toIHtmlRow [ 
                        str sai
                        span [_id "commonName"] [ str commonName ]
                    ]
                ]
            ]
            
            yield p [] [ str "S4 Flocs:" ]

            yield p [] [ htmlTable answers ] 

            yield p [] [
                a [ _href "/" ] [ str "Back" ]
            ]
        ] |> makePage "S4 Mappings"

    let internalErrorPage (errMsg : string) : XmlNode =
        [
            yield p [] ["An internal error has occured:" |> str]

            yield p [] [errMsg |> str]

        ] |> makePage "Internal Error"