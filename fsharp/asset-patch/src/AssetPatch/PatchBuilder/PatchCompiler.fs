// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.PatchBuilder


module PatchCompiler =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.EntityTypes
    open AssetPatch.Base.CompilerMonad    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.PatchBuilder.Hierarchy
    open AssetPatch.PatchBuilder.Renamer
    open AssetPatch.PatchBuilder.Emitter
    open AssetPatch.PatchBuilder.PatchGen
    

    type ClassTemplate<'a> = 'a -> Class
   
    type FunctionTemplate<'a> = 'a -> Function 
   
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

    /// Generate patches for a new level 2 function and its subordinates
    let compileFunctionPatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (user : string) 
                                         (template : FunctionTemplate<'hole>)
                                         (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit, 'env> = 
        let compile1 (path, func) = 
            functionRename func >>= functionEmit path
        compile {
            let (worklist1 : (FuncLocPath * Function) list) = 
                applyTemplate worklist template
            let! results =  forM worklist1 compile1
            do! generatePatches outputDirectory filePrefix user (concatResults results)
            return ()
        }