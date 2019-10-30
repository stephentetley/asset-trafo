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
        


    type FuncLoc = 
        { Path : FuncLocPath
          Description : string
          ObjectType : string
          Classes : Class list
        }

        interface IAddClass<FuncLoc> with
            member x.AddClass(c1 : Class) = 
                let cs = x.Classes @ [c1]
                { x with Classes = cs }


    [<Struct>]
    type EquipmentCode = 
        | EquipmentCode of string

    type Equipment = 
        { Code : EquipmentCode
          FuncLoc : FuncLocPath
          Description : string
          ObjectType : string
          Classes : Class list
        }
        interface IAddClass<Equipment> with
            member x.AddClass(c1 : Class) = 
                let cs = x.Classes @ [c1]
                { x with Classes = cs }


    let (<!<) (claz : Class) (charValue : Characteristic) : Class = 
        let vs = claz.Characteritics @ [charValue]
        { claz with Characteritics = vs }

    let (<<!<) (parent : 'T) (claz : Class) : 'T when 'T :> IAddClass<'T> = 
        (parent :> IAddClass<'T>).AddClass(claz)