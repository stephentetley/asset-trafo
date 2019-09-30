// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AideSync


module Datatypes2 =
    
    
    type Property = 
        { PropertyName : string 
          AibValue : string
          AideValue : string }

        member x.IsMatch 
            with get () : bool = x.AibValue = x.AideValue


    type Attribute = 
        { AttributeName : string 
          AibValue : string
          AideValue : string }

        member x.IsMatch 
            with get () : bool = x.AibValue = x.AideValue

    type Structure<'Label> = 
        | StructureNode of label : 'Label * kids : Structure<'Label> list



    type Delta<'TLeft, 'TRight> = 
        | InLeft of 'TLeft
        | Match of left : 'TLeft * right : 'TRight
        | Difference of left : 'TLeft * right : 'TRight
        | InRight of 'TRight


  

    
    type AiFlocNode = 
        { AssetId : int64
          Reference : string
          ShortName: string 
          CommonName : string 
        }
        // We want to be able to sort top down (priority go down, 
        // rather than go next).
        // The simplest way to do this is hack the key so '/' has 
        // priority over ' '.
        member v.PathKey 
            with get() : string  = 
                v.CommonName.Replace(' ', '?')

    type AideFlocNode = 
        { AideAssetId : int64        
          Reference : string
          ShortName : string 
          CommonName : string
        }
        // We want to be able to sort top down (priority go down, 
        // rather than go next).
        // The simplest way to do this is hack the key so '/' has 
        // priority over ' '.
        member v.PathKey 
            with get() : string  = 
                v.CommonName.Replace(' ', '?')


    type InitialDelta = Delta<AiFlocNode, AideFlocNode>

    type ChangeRequestInfo = 
        { ChangeRequestId : int64
          RequestType : string
          Status : string
          Comment : string
          RequestTime : System.DateTime
        }

    type ChangeRequest = 
        { Info : ChangeRequestInfo 
          Changes : Structure<unit> list } // TODO - obvs. not unit

    type ChangeSchemeInfo = 
        { SchemeId : int64
          SchemeCode : string
          Name : string
          SolutionProvider : string
        }

    type ChangeScheme = 
        { Info : ChangeSchemeInfo
          StructureChanges : ChangeRequest list
        }
