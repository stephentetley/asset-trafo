// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module PatchCompiler =
    
    open System.IO

    // Open first so common names get overridden
    open AssetPatch.TemplatePatcher.Template

    open AssetPatch.Base.CompilerMonad    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Hierarchy
    open AssetPatch.TemplatePatcher.Emitter
    open AssetPatch.TemplatePatcher.EquiIndexing
    open AssetPatch.TemplatePatcher.PatchGen
    

    
    
    let evalTemplate (code : Template.Template<'a>) : CompilerMonad<'a> = 
        compile {
            let! templateEnv = asks id
            return! liftResult (Template.runTemplate templateEnv code)
        }

    let applyTemplate (xs: ('id * 'b) list ) 
                        (template: 'b -> Template.Template<'c>) : CompilerMonad<('id * 'c) list> = 
        forM xs (fun (name, x) -> evalTemplate (template x) >>=  fun a -> mreturn (name, a))


    

    // This is not printed...
    let private anonEquipment (code : string) (clazz: S4Class) : S4Equipment= 
        { 
            EquipmentId = code
            Description = ""
            Category = "I"
            ObjectType = ""
            Manufacturer = None
            Model = None
            SerialNumber = None
            ConstructionYear = None
            ConstructionMonth = None
            Classes = [clazz] 
            SuboridnateEquipment = [] 
        }




    /// Generate Class and Char patches to update existing equipment.
    let compileClassEquiValuaEquiPatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (template : Class1<'hole>)
                                         (worklist : (string * 'hole) list)
                                            : CompilerMonad<unit> = 
        compile {
            let! worklist1 = applyTemplate worklist template
            let! results = 
                forM worklist1 (fun (name, clazz) -> equipmentEmitClassValuas (anonEquipment name clazz))
            do! generatePatches outputDirectory filePrefix (concatResults results)
            return ()
        }

    /// Generate Class and Char patches to update existing equipment.
    let compileClassFlocValuaFlocPatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (template : Class1<'hole>)
                                         (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit> = 
        compile {
            let! worklist1 = applyTemplate worklist template
            let! results = 
                forM worklist1 (fun (path, clazz) -> funcLocPathEmitClassValuas path [clazz])
            do! generatePatches outputDirectory filePrefix (concatResults results)
            return ()
        }


    /// Generate patches for a new level 3 process group and its subordinates
    let compileHierarchyPatches (outputDirectory : string)
                                (filePrefix : string)
                                (compile1 : (FuncLocPath * 'object) -> CompilerMonad<EmitterResults>)
                                (template : 'hole -> Template.Template<'object>)
                                (worklist : (FuncLocPath * 'hole) list) : CompilerMonad<unit> =         
        compile {
            let! worklist1 = applyTemplate worklist template
            let! results =  forM worklist1 compile1
            do! generatePatches outputDirectory filePrefix (concatResults results)
            return ()
        }

    /// Generate patches for a new level 2 function and its subordinates
    let compileComponentPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Component1<'hole>)
                                (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit> = 
        let compile1 (path, comp) = componentEmit path comp
        compileHierarchyPatches outputDirectory filePrefix compile1 template worklist



    /// Generate patches for a new level 2 function and its subordinates
    let compileItemPatches (outputDirectory : string)
                            (filePrefix : string)
                            (template : Item1<'hole>)
                            (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit> = 
        let compile1 (path, item) = itemEmit path item
        compileHierarchyPatches outputDirectory filePrefix compile1 template worklist



    /// Generate patches for a new level 2 function and its subordinates
    let compileAssemblyPatches (outputDirectory : string)
                              (filePrefix : string)
                              (template : Assembly1<'hole>)
                              (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit> = 
        let compile1 (path, asmb) = assemblyEmit path asmb
        compileHierarchyPatches outputDirectory filePrefix compile1 template worklist


    /// Generate patches for a new level 2 function and its subordinates
    let compileSystemPatches (outputDirectory : string)
                              (filePrefix : string)
                              (template : System1<'hole>)
                              (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit> = 
        let compile1 (path, syst) = systemEmit path syst
        compileHierarchyPatches outputDirectory filePrefix compile1 template worklist


    /// Generate patches for a new level 2 function and its subordinates
    let compileProcessPatches (outputDirectory : string)
                              (filePrefix : string)
                              (template : Process1<'hole>)
                              (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit> = 
        let compile1 (path, prcs) = processEmit path prcs
        compileHierarchyPatches outputDirectory filePrefix compile1 template worklist


    /// Generate patches for a new level 2 function and its subordinates
    let compileProcessGroupPatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (template : ProcessGroup1<'hole>)
                                         (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit> = 
        let compile1 (path, prcg) = processGroupEmit path prcg
        compileHierarchyPatches outputDirectory filePrefix compile1 template worklist


    /// Generate patches for a new level 2 function and its subordinates
    let compileFunctionPatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (template : Function1<'hole>)
                                         (worklist : (FuncLocPath * 'hole) list)
                                            : CompilerMonad<unit> = 
        let compile1 (path, func) = functionEmit path func
        compileHierarchyPatches outputDirectory filePrefix compile1 template worklist


    /// Generate patches for a new level 1 site and its subordinates
    let compileSitePatches (outputDirectory : string)
                            (filePrefix : string)
                            (template : Site1<'hole>)
                            (worklist : 'hole list) : CompilerMonad<unit> = 
        let compile1 = evalTemplate >=> siteEmit
        compile {
            let worklist1 = List.map template worklist
            let! results = forM worklist1 compile1
            do! generatePatches outputDirectory filePrefix (concatResults results)
            return ()
        }

    /// Generate patches for a new level 1 site and its subordinates
    let materializeEquiClassValuaPatches (outputDirectory : string) : CompilerMonad<unit> = 
        compile {
            let xlsxFile = Path.Combine(outputDirectory, "EquiIndexing.xlsx")
            let! substs = readEquiIndexingSheet xlsxFile
            let sources = Directory.GetFiles(path = outputDirectory, searchPattern = "*.apch") |> List.ofArray
            do! mapMz (materializeEquiFile substs) sources
            return ()
        }