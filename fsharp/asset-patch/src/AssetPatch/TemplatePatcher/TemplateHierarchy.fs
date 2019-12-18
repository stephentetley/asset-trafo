// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module TemplateHierarchy =
    
    open System

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CommonTypes
    

    type S4Characteristic = 
        { Name : string
          Value : ValuaValue 
        }

    
    let sortedCharacteristics (source : S4Characteristic list) : (S4Characteristic list) list= 
        source
            |> List.sortBy (fun x -> x.Name)
            |> List.groupBy (fun x -> x.Name)               
            |> List.map snd


    type S4Class = 
        { ClassName : string          
          Characteristics : S4Characteristic list
        }


    /// EquipmentId may be a dollar number
    type S4Equipment = 
        { EquipmentId : string option
          Description : string       
          Category : string
          ObjectType : string
          Manufacturer : string option
          Model : string option
          SerialNumber : string option
          ConstructionYear : uint16 option
          ConstructionMonth : uint8 option
          Classes : S4Class list
          SuboridnateEquipment : S4Equipment list
        }


    type FuncLocProperties = 
        { StartupDate : DateTime 
          ObjectStatus : string         
          StructureIndicator : string
          MaintenancePlant : uint32
          ControllingArea : uint32
          CompanyCode : uint32
          Currency : string
        }

    
    type S4Component = 
        { FuncLoc : FuncLocPath
          FlocProperties : FuncLocProperties
          Description : string
          ObjectType : string
          Classes : S4Class list
          Equipment : S4Equipment list        
        }


    type S4Item = 
        { FuncLoc : FuncLocPath
          FlocProperties : FuncLocProperties
          Description : string
          ObjectType : string
          Classes : S4Class list
          Components : S4Component list
          Equipment : S4Equipment list        
        }


    type S4Assembly = 
        { FuncLoc : FuncLocPath
          FlocProperties : FuncLocProperties
          Description : string
          ObjectType : string
          Classes : S4Class list
          Items : S4Item list
          Equipment : S4Equipment list        
        }

    type S4System = 
        { FuncLoc : FuncLocPath
          FlocProperties : FuncLocProperties
          Description : string
          ObjectType : string
          Classes : S4Class list
          Assemblies : S4Assembly list
          Equipment : S4Equipment list        
        }

    type S4Process = 
        { FuncLoc : FuncLocPath
          FlocProperties : FuncLocProperties
          Description : string
          ObjectType : string
          Classes : S4Class list
          Systems : S4System list     
        }

    type S4ProcessGroup = 
        { FuncLoc : FuncLocPath
          FlocProperties : FuncLocProperties
          Description : string
          ObjectType : string
          Classes : S4Class list
          Processes : S4Process list    
        }

    type S4Function = 
        { FuncLoc : FuncLocPath
          FlocProperties : FuncLocProperties
          Description : string
          ObjectType : string
          Classes : S4Class list
          ProcessGroups : S4ProcessGroup list    
        }

    type S4Site = 
        { FuncLoc : FuncLocPath
          FlocProperties : FuncLocProperties
          Description : string
          ObjectType : string
          Classes : S4Class list
          Functions : S4Function list    
        }

