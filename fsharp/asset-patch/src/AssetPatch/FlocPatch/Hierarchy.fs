// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocPatch



module Hierarchy =
    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.EntityTypes
    

    let readFloc (s : string) : FuncLocPath = 
        FuncLocPath.Create(s)

    type Characteristic = 
        | Characteristic of name : string * value : string


    type Class = 
        { ClassName : string          
          ClassInt : uint32
          Characteritics : Characteristic list
        }

    type IAddClass<'T> = 
        abstract AddClass : Class -> 'T
        


    type Floc = 
        { Segment : string
          Description : string
          ObjectType : string
          Classes : Class list
        }


    type EquipmentCode = 
        | EquipmentCode of string
        | EquipmentAnon

    type Equipment = 
        { Code : EquipmentCode
          Description : string
          ObjectType : string
          Classes : Class list
        }


    let _class (name : string) (number : uint32) (values : Characteristic list) : Class = 
        { ClassName = name
          ClassInt = number
          Characteritics = values }

    let _char (name : string) (value : string) : Characteristic = 
        Characteristic(name, value)

    let _floc (segment: string) (description : string) (objectType : string)
                (classes : Class list) : Floc = 
        { Segment = segment
          Description = description
          ObjectType = objectType
          Classes = classes }

    let _equipment (description : string) (objectType : string)
                    (classes : Class list) : Equipment = 
        { Code = EquipmentAnon
          Description = description
          ObjectType = objectType
          Classes = classes }