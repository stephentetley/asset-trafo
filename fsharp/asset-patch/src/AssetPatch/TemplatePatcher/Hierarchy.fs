// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module Hierarchy =
    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.EntityTypes
    

    type S4Characteristic = 
        { Name : string
          Value : string 
        }

    type S4Class = 
        { ClassName : string          
          ClassInt : uint32
          Characteritics : S4Characteristic list
        }



    type S4Equipment = 
        { EquipmentId : string option
          Description : string
          ObjectType : string
          Manufacturer : string option
          Model : string option
          Classes : S4Class list
          SuboridnateEquipment : S4Equipment list
        }

    
    type S4Component = 
        { FuncLocSegment : FuncLocSegment
          Classes : S4Class list
          Equipment : S4Equipment list        
        }


    type S4Item = 
        { FuncLocSegment : FuncLocSegment
          Classes : S4Class list
          Components : S4Component list
          Equipment : S4Equipment list        
        }


    type S4Assembly = 
        { FuncLocSegment : FuncLocSegment
          Classes : S4Class list
          Items : S4Item list
          Equipment : S4Equipment list        
        }

    type S4System = 
        { FuncLocSegment : FuncLocSegment
          Classes : S4Class list
          Assemblies : S4Assembly list
          Equipment : S4Equipment list        
        }

    type S4Process = 
        { FuncLocSegment : FuncLocSegment
          Classes : S4Class list
          Systems : S4System list     
        }

    type S4ProcessGroup = 
        { FuncLocSegment : FuncLocSegment
          Classes : S4Class list
          Processes : S4Process list    
        }

    type S4Function = 
        { FuncLocSegment : FuncLocSegment
          Classes : S4Class list
          ProcessGroups : S4ProcessGroup list    
        }

    type S4Site = 
        { FuncLocSegment : FuncLocSegment
          Classes : S4Class list
          Functions : S4Function list    
        }

    /// *** If we had monadic templates (Reader) we could have a lot more
    /// *** flexibility for inheriting attributes

    // let _segment (token : string) 
    //              (description : string) 
    //              (objectType : string) : FuncLocSegment = 
    //     { Name = token
    //       Description = description
    //       ObjectType = objectType 
    //     }
    
    // let _characteristic (name : string) (value : string) : S4Characteristic = 
    //     { Name = name
    //       Value = value
    //     }

    // let _class (name : string) (number : uint32) (values : S4Characteristic list) : S4Class = 
    //     { ClassName = name
    //       ClassInt = number
    //       Characteritics = values }


    // let _equipment (description : string) (objectType : string)
    //                 (classes : S4Class list) 
    //                 (subordinateEquipment : S4Equipment list) : S4Equipment = 
    //     // EquipmentId is filled in by a compiler pass
    //     { EquipmentId = None
    //       Description = description
    //       ObjectType = objectType
    //       Manufacturer = None
    //       Model = None
    //       Classes = classes 
    //       SuboridnateEquipment = subordinateEquipment }

    
    // let _component (token : string) (description : string) (objectType : string)
    //                (classes : S4Class list) (equipment : S4Equipment list) : S4Component = 
    //     { FuncLocSegment = _segment token description objectType
    //       Classes = classes 
    //       Equipment = equipment }

    // let _item (token : string) (description : string) (objectType : string)
    //             (classes : S4Class list) 
    //             (components : S4Component list) (equipment : S4Equipment list) : S4Item = 
    //     { FuncLocSegment = _segment token description objectType
    //       Classes = classes 
    //       Components = components
    //       Equipment = equipment }

    // let _assembly (token : string) (description : string) (objectType : string)
    //                 (classes : S4Class list) (items : S4Item list) 
    //                 (equipment : S4Equipment list) : S4Assembly = 
    //     { FuncLocSegment = _segment token description objectType
    //       Classes = classes 
    //       Items = items
    //       Equipment = equipment }

    // let _system (token : string) (description : string) (objectType : string)
    //             (classes : S4Class list) (assemblies : S4Assembly list) 
    //             (equipment : S4Equipment list) : S4System = 
    //     { FuncLocSegment = _segment token description objectType
    //       Classes = classes 
    //       Assemblies = assemblies
    //       Equipment = equipment }

    // let _process (token : string) (description : string) (objectType : string)
    //                 (classes : S4Class list) 
    //                 (systems : S4System list) : S4Process = 
    //     { FuncLocSegment = _segment token description objectType
    //       Classes = classes 
    //       Systems = systems }

    // let _processGroup (token : string) (description : string) (objectType : string)    
    //                     (classes : S4Class list) 
    //                     (processes : S4Process list) : S4ProcessGroup = 
    //     { FuncLocSegment = _segment token description objectType
    //       Classes = classes 
    //       Processes = processes }

    // let _function (token : string) (description : string) (objectType : string)
    //               (classes : S4Class list) 
    //               (processGroups : S4ProcessGroup list) : S4Function = 
    //     { FuncLocSegment = _segment token description objectType
    //       Classes = classes 
    //       ProcessGroups = processGroups }

    // let _site (token : string) (description : string) (objectType : string)
    //             (classes : S4Class list) 
    //             (functions : S4Function list) : S4Site = 
    //     { FuncLocSegment = _segment token description objectType
    //       Classes = classes 
    //       Functions = functions }

