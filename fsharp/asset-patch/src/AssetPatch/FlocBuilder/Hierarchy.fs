// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocBuilder



module Hierarchy =
    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.EntityTypes
    

    type Characteristic = 
        | Characteristic of name : string * value : string


    type Class = 
        { ClassName : string          
          ClassInt : uint32
          Characteritics : Characteristic list
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

    
    type Component = 
        { FuncLocSegment : FuncLocSegment
          Classes : Class list
          Equipment : Equipment list        
        }


    type Item = 
        { FuncLocSegment : FuncLocSegment
          Classes : Class list
          Components : Component list
          Equipment : Equipment list        
        }


    type Assembly = 
        { FuncLocSegment : FuncLocSegment
          Classes : Class list
          Items : Item list
          Equipment : Equipment list        
        }

    type System = 
        { FuncLocSegment : FuncLocSegment
          Classes : Class list
          Assemblies : Assembly list
          Equipment : Equipment list        
        }

    type Process = 
        { FuncLocSegment : FuncLocSegment
          Classes : Class list
          Systems : System list     
        }

    type ProcessGroup = 
        { FuncLocSegment : FuncLocSegment
          Classes : Class list
          Processes : Process list    
        }

    type Function = 
        { FuncLocSegment : FuncLocSegment
          Classes : Class list
          ProcessGroups : ProcessGroup list    
        }

    type Site = 
        { FuncLocSegment : FuncLocSegment
          Classes : Class list
          Functions : Function list    
        }


    let _segment (token : string) 
                 (description : string) 
                 (objectType : string) : FuncLocSegment = 
        { Name = token
          Description = description
          ObjectType = objectType 
        }
    
    let _char (name : string) (value : string) : Characteristic = 
        Characteristic(name, value)

    let _class (name : string) (number : uint32) (values : Characteristic list) : Class = 
        { ClassName = name
          ClassInt = number
          Characteritics = values }


    let _equipment (description : string) (objectType : string)
                    (classes : Class list) : Equipment = 
        { Code = EquipmentAnon
          Description = description
          ObjectType = objectType
          Classes = classes }

    
    let _component (segment: FuncLocSegment) 
                   (classes : Class list) (equipment : Equipment list) : Component = 
        { FuncLocSegment = segment
          Classes = classes 
          Equipment = equipment }

    let _item (segment: FuncLocSegment) 
                (classes : Class list) 
                (components : Component list) (equipment : Equipment list) : Item = 
        { FuncLocSegment = segment
          Classes = classes 
          Components = components
          Equipment = equipment }

    let _assembly (segment: FuncLocSegment) 
                    (classes : Class list) 
                    (items : Item list) (equipment : Equipment list) : Assembly = 
        { FuncLocSegment = segment
          Classes = classes 
          Items = items
          Equipment = equipment }

    let _system (segment: FuncLocSegment) 
                (classes : Class list) 
                (assemblies : Assembly list) (equipment : Equipment list) : System = 
        { FuncLocSegment = segment
          Classes = classes 
          Assemblies = assemblies
          Equipment = equipment }

    let _process (segment: FuncLocSegment) 
                    (classes : Class list) 
                    (systems : System list) : Process = 
        { FuncLocSegment = segment
          Classes = classes 
          Systems = systems }

    let _processGroup (segment: FuncLocSegment) 
                        (classes : Class list) 
                        (processes : Process list) : ProcessGroup = 
        { FuncLocSegment = segment
          Classes = classes 
          Processes = processes }

    let _function (segment: FuncLocSegment) 
                  (classes : Class list) 
                  (processGroups : ProcessGroup list) : Function = 
        { FuncLocSegment = segment
          Classes = classes 
          ProcessGroups = processGroups }

    let _site (segment: FuncLocSegment) 
                (classes : Class list) 
                (functions : Function list) : Site = 
        { FuncLocSegment = segment
          Classes = classes 
          Functions = functions }

