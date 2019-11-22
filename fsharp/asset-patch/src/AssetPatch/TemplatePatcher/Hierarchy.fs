// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module Hierarchy =
    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.PatchTypes
    

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
          Category : string
          ObjectType : string
          Manufacturer : string option
          Model : string option
          SerialNumber : string option
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

