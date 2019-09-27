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

    
    
    
    let resultsPage (mappings : FlocMapping list) : XmlNode =
        let answer1 (aibRef : string) 
                    (commonName : string) 
                    (answer : LookupAnswer) : XmlNode list = 
            let answerTop = 
                match answer with
                | EquipmentAns ans -> 
                    [ 
                        td [] [ans.ParentFloc.ToString() |> str]
                        td [] [ans.EquipmentId.ToString() |> str]
                    ]
                | FlocAns ans -> 
                    [
                        td [_colspan "2"] [ans.S4Floc.ToString() |> str]
                    ]
                | LookupFail -> 
                    [
                        td [_colspan "2"] ["Lookup not found" |> str]
                    ]
            let answerPath = 
                match answer with
                | EquipmentAns ans -> ans.ParentDescPath |> str
                | FlocAns ans -> ans.DescriptionPath |> str
                | LookupFail -> "&nbsp;" |> rawText
            [ 
                tr [] [ 
                    yield td [] [aibRef |> str]
                    yield td [] ["&rArr;" |> rawText]
                    yield! answerTop
                ]

                tr [_class "explanation"] [
                    td [_colspan "4"] [commonName |> str]
                ]

                tr [_class "explanation"] [
                    td [] ["&nbsp;" |> rawText]
                    td [] ["&rArr;" |> rawText]
                    td [_colspan "2"] [answerPath]
                ]
            ]
            
        // One Floc mapping generates
        let mapping1 (mapping : FlocMapping) : XmlNode list = 
            List.map (answer1 mapping.AibReference mapping.AibCommonName) mapping.MappingAnswers
                |> List.concat
        [            
            yield p [] [ 
                table [] [
                    thead [] [
                        tr [] [ 
                            th [_style "width:20%"] ["Aib Code" |> str]
                            th [_style "width:5%"] ["&nbsp;" |> rawText]
                            th [_style "width:50%"] ["Floc" |> str]
                            th [_style "width:25%"] ["Equipment Number" |> str]
                        ]
                    ]
                    tbody [] [
                        yield! List.map mapping1 mappings |> List.concat
                    ]
                
                ]
            ]
            yield p [] [
                a [ _href "/" ] [ str "Back" ]
            ]
        ] |> makePage "S4 Mappings"

    let internalErrorPage (errMsg : string) : XmlNode =
        [
            yield p [] ["An internal error has occured:" |> str]

            yield p [] [errMsg |> str]

        ] |> makePage "Internal Error"