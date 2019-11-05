// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module PatchCompiler =
    

    open AssetPatch.Base.EntityTypes
    open AssetPatch.Base.CompilerMonad    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Hierarchy
    open AssetPatch.TemplatePatcher.Renamer
    open AssetPatch.TemplatePatcher.Emitter
    open AssetPatch.TemplatePatcher.PatchGen
    

    type ClassTemplate<'a> = 'a -> Class
   
    type ComponentTemplate<'a> = 'a -> Component
    
    type ItemTemplate<'a> = 'a -> Item 
    
    type AssemblyTemplate<'a> = 'a -> Assembly 
    
    type SystemTemplate<'a> = 'a -> System 

    type ProcessTemplate<'a> = 'a -> Process 

    type ProcessGroupTemplate<'a> = 'a -> ProcessGroup 
    
    type FunctionTemplate<'a> = 'a -> Function 
    
    type SiteTemplate<'a> = 'a -> Site
   
    let applyTemplate (xs: ('id * 'b) list ) (template: 'b -> 'c) : ('id * 'c) list = 
        xs |> List.map (fun (name,x) -> (name, template x))



    let private anonEquipment (code : string) (clazz: Class) = 
        let equip1 = _equipment "" "" [clazz] []
        { equip1 with EquipmentId = Some code }



    /// Generate Class and Char patches to update existing equipment.
    let compileClassEquiValuaEquiPatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (user : string) 
                                         (template : ClassTemplate<'hole>)
                                         (worklist : (EquipmentCode * 'hole) list)
                                            : CompilerMonad<unit, 'env> = 
        compile {
            let worklist1 = applyTemplate worklist template
            let! results = 
                forM worklist1 (fun (name, clazz) -> equipmentEmitClassValuas (anonEquipment name.Code clazz))
            do! generatePatches outputDirectory filePrefix user (concatResults results)
            return ()
        }

    /// Generate Class and Char patches to update existing equipment.
    let compileClassFlocValuaFlocPatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (user : string) 
                                         (template : ClassTemplate<'hole>)
                                         (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit, 'env> = 
        compile {
            let worklist1 = applyTemplate worklist template
            let! results = 
                forM worklist1 (fun (path, clazz) -> funcLocPathEmitClassValuas path [clazz])
            do! generatePatches outputDirectory filePrefix user (concatResults results)
            return ()
        }


    /// Generate patches for a new level 3 process group and its subordinates
    let compileHierarchyPatches (outputDirectory : string)
                                (filePrefix : string)
                                (user : string) 
                                (compile1 : (FuncLocPath * 'object) -> CompilerMonad<EmitterResults, 'env>)
                                (template : 'hole -> 'object)
                                (worklist : (FuncLocPath * 'hole) list) : CompilerMonad<unit, 'env> =         
        compile {
            let worklist1 = applyTemplate worklist template
            let! results =  forM worklist1 compile1
            do! generatePatches outputDirectory filePrefix user (concatResults results)
            return ()
        }

    /// Generate patches for a new level 2 function and its subordinates
    let compileComponentPatches (outputDirectory : string)
                                (filePrefix : string)
                                (user : string) 
                                (template : ComponentTemplate<'hole>)
                                (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit, 'env> = 
        let compile1 (path, func) = componentRename func >>= componentEmit path
        compileHierarchyPatches outputDirectory filePrefix user compile1 template worklist



    /// Generate patches for a new level 2 function and its subordinates
    let compileItemPatches (outputDirectory : string)
                            (filePrefix : string)
                            (user : string) 
                            (template : ItemTemplate<'hole>)
                            (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit, 'env> = 
        let compile1 (path, func) = itemRename func >>= itemEmit path
        compileHierarchyPatches outputDirectory filePrefix user compile1 template worklist



    /// Generate patches for a new level 2 function and its subordinates
    let compileAssemblyPatches (outputDirectory : string)
                              (filePrefix : string)
                              (user : string) 
                              (template : AssemblyTemplate<'hole>)
                              (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit, 'env> = 
        let compile1 (path, func) = assemblyRename func >>= assemblyEmit path
        compileHierarchyPatches outputDirectory filePrefix user compile1 template worklist


    /// Generate patches for a new level 2 function and its subordinates
    let compileSystemPatches (outputDirectory : string)
                              (filePrefix : string)
                              (user : string) 
                              (template : SystemTemplate<'hole>)
                              (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit, 'env> = 
        let compile1 (path, func) = systemRename func >>= systemEmit path
        compileHierarchyPatches outputDirectory filePrefix user compile1 template worklist


    /// Generate patches for a new level 2 function and its subordinates
    let compileProcessPatches (outputDirectory : string)
                              (filePrefix : string)
                              (user : string) 
                              (template : ProcessTemplate<'hole>)
                              (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit, 'env> = 
        let compile1 (path, func) = processRename func >>= processEmit path
        compileHierarchyPatches outputDirectory filePrefix user compile1 template worklist


    /// Generate patches for a new level 2 function and its subordinates
    let compileProcessGroupPatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (user : string) 
                                         (template : ProcessGroupTemplate<'hole>)
                                         (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit, 'env> = 
        let compile1 (path, func) = processGroupRename func >>= processGroupEmit path
        compileHierarchyPatches outputDirectory filePrefix user compile1 template worklist


    /// Generate patches for a new level 2 function and its subordinates
    let compileFunctionPatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (user : string) 
                                         (template : FunctionTemplate<'hole>)
                                         (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit, 'env> = 
        let compile1 (path, func) = functionRename func >>= functionEmit path
        compileHierarchyPatches outputDirectory filePrefix user compile1 template worklist

    /// Generate patches for a new level 1 site and its subordinates
    let compileSitePatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (user : string) 
                                         (template : SiteTemplate<'hole>)
                                         (worklist : 'hole list)
                                            : CompilerMonad<unit, 'env> = 
        let compile1 = siteRename >=> siteEmit
        compile {
            let worklist1=  List.map template worklist
            let! results =  forM worklist1 compile1
            do! generatePatches outputDirectory filePrefix user (concatResults results)
            return ()
        }