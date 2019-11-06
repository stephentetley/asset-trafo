// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module Renamer =

    open AssetPatch.Base
    open AssetPatch.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Hierarchy
    
    
    let equipmentRename (equipment1 : Equipment) : CompilerMonad<Equipment> = 
        let getName (x : Equipment) = 
            match x.EquipmentId with
            | None -> newEquipmentName ()
            | Some a -> mreturn a

        let rec work (xs : Equipment list) 
                    (cont : Equipment list -> CompilerMonad<Equipment list>) = 
            match xs with
            | [] -> mreturn []
            | x :: rest -> 
                compile {
                    let! name = getName x                        
                    return! 
                        work x.SuboridnateEquipment (fun kids -> 
                        let e1 = { x with EquipmentId = Some name; SuboridnateEquipment = kids } 
                        work rest (fun sibs -> cont (e1 :: sibs)))
                }
    
        compile {
            let! name = getName equipment1
            let! kids = work equipment1.SuboridnateEquipment mreturn
            return { equipment1 with EquipmentId = Some name; SuboridnateEquipment = kids }
        }
    

    let componentRename (compo : Component) : CompilerMonad<Component> = 
        compile {
            let! kids = mapM equipmentRename compo.Equipment
            return { compo with Equipment = kids }
        }


    let itemRename (item : Item) : CompilerMonad<Item> = 
        compile {
            let! components = mapM componentRename item.Components
            let! equipment = mapM equipmentRename item.Equipment
            return { item with Components = components; Equipment = equipment }
        }
    

    let assemblyRename (assembly : Assembly) : CompilerMonad<Assembly> = 
        compile {
            let! items = mapM itemRename assembly.Items
            let! equipment = mapM equipmentRename assembly.Equipment
            return { assembly with Items = items; Equipment = equipment }
        }


    let systemRename (system : System) : CompilerMonad<System> = 
        compile {
            let! assemblies = mapM assemblyRename system.Assemblies
            let! equipment = mapM equipmentRename system.Equipment
            return { system with Assemblies = assemblies; Equipment = equipment }
        }


    let processRename (proc : Process) : CompilerMonad<Process> = 
        compile {
            let! systems = mapM systemRename proc.Systems
            return { proc with Systems = systems }
        }


    let processGroupRename (procGroup : ProcessGroup) : CompilerMonad<ProcessGroup> = 
        compile {
            let! procs = mapM processRename procGroup.Processes
            return { procGroup with Processes = procs }
        }


    let functionRename (func : Function) : CompilerMonad<Function> = 
        compile {
            let! procGroups = mapM processGroupRename func.ProcessGroups
            return { func with ProcessGroups = procGroups }
        }

    
    let siteRename (site : Site) : CompilerMonad<Site> = 
        compile {
            let! funcs = mapM functionRename site.Functions
            return { site with Functions = funcs }
        }
