// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Web

module Base = 
    
    open System.Text.RegularExpressions

    open Giraffe.GiraffeViewEngine

    type IHtmlRow = 
        abstract ToTr : XmlAttribute list -> XmlNode

    let inline toIHtmlRow1 (xmlNode : XmlNode) = 
        { new IHtmlRow with
            member __.ToTr (attrs : XmlAttribute list) = tr attrs [td [] [xmlNode]] }

    let inline toIHtmlRow (xmlNodes : XmlNode list) = 
        { new IHtmlRow with
            member __.ToTr (attrs : XmlAttribute list) = 
                let cells = List.map (fun x -> td [] [x]) xmlNodes
                tr attrs cells }


    let htmlTable (items : #IHtmlRow list) : XmlNode = 
        let makeRow (ix:int) (item : #IHtmlRow) = 
            if (ix+1) % 2 = 0 then item.ToTr [_class "even"] else item.ToTr []
        table [] (List.mapi makeRow items)
        

    let isPliCode (code : string) : bool = 
        Regex.IsMatch(input=code, pattern = "^PLI\d{8}$")

    let isAibCode (code : string) : bool = 
        Regex.IsMatch(input=code, pattern = "^[A-Z]{3}\d{8}$")
        

