// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace FlocMapping.Web

module Base = 
    
    open Giraffe.GiraffeViewEngine

    type IHtmlRow = 
        abstract ToTr : XmlAttribute list -> XmlNode

    let htmlTable (items : #IHtmlRow list) : XmlNode = 
        let makeRow (ix:int) (item : #IHtmlRow) = 
            if (ix+1) % 2 = 0 then item.ToTr [_class "even"] else item.ToTr []
        table [] (List.mapi makeRow items)
        


        

