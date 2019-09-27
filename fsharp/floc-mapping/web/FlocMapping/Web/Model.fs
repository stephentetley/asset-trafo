// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Web

module Model = 
    
    open Giraffe.GiraffeViewEngine

    open FlocMapping.S4Basis
    open FlocMapping.Web.Base
    
    [<CLIMutable>]
    type ReferencesForm =
        { SingleReference : string
          MultipleReferences : string }
    
    type FlocAnswer = 
        { S4Floc : Floc 
          Description : string
          DescriptionPath : string }

        interface IHtmlRow with
            member x.ToTr (attrs : XmlAttribute list) : XmlNode = 
                tr attrs [
                    td [] [x.S4Floc.ToString() |> str] 
                    td [_id "commonName"] [x.Description |> str]
                ]

              


    type EquipmentAnswer = 
        { ParentFloc : Floc 
          ParentDesc : string
          ParentDescPath : string 
          EquipmentId : int64 
          EquipmentDesc : string }

        interface IHtmlRow with
            member x.ToTr (attrs : XmlAttribute list) : XmlNode = 
                tr attrs [
                    td [] [x.ParentFloc.ToString() |> str] 
                    td [_id "commonName"] [x.ParentDesc |> str]
                    td [] [x.EquipmentId.ToString() |> str] 
                    td [_id "commonName"] [x.EquipmentDesc |> str]
                  ]


    type LookupAnswer = 
        | EquipmentAns of EquipmentAnswer
        | FlocAns of FlocAnswer
        | LookupFail 

        interface IHtmlRow with
            member x.ToTr (attrs : XmlAttribute list) : XmlNode = 
                match x with
                | EquipmentAns ans -> (ans :> IHtmlRow).ToTr attrs
                | FlocAns ans -> (ans :> IHtmlRow).ToTr attrs
                | LookupFail -> tr attrs [td [] ["Lookup failed" |> str]]

   
    type FlocMapping = 
        { AibReference : string 
          AibCommonName : string
          MappingAnswers : LookupAnswer list }