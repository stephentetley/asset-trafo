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

             
    type EquipmentAnswer = 
        { ParentFloc : Floc 
          ParentDesc : string
          ParentDescPath : string 
          EquipmentId : int64 
          EquipmentDesc : string }


    type LookupAnswer = 
        | EquipmentAns of EquipmentAnswer
        | FlocAns of FlocAnswer
        | LookupFail 

   
    type FlocMapping = 
        { AibReference : string 
          AibCommonName : string
          MappingAnswers : LookupAnswer list }

