// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module PatchCompiler =
    
    open System.IO

    // Open first so common names get overridden
    open AssetPatch.TemplatePatcher.Template

    open AssetPatch.Base.FuncLocPath 
    open AssetPatch.TemplatePatcher.Hierarchy
    open AssetPatch.TemplatePatcher.CompilerMonad   
    open AssetPatch.TemplatePatcher.EquiIndexing
    open AssetPatch.TemplatePatcher.EmitEquipment
    open AssetPatch.TemplatePatcher.EmitFuncLoc
    open AssetPatch.TemplatePatcher.Emitter
    


    // ************************************************************************
    // Compile Class * Valua patches to update exting Flocs and Equipment...


    let private applyTemplate (path : FuncLocPath) 
                                (xs: ('name * 'b) list ) 
                                (template: 'b -> Template.Template<'c>) : CompilerMonad<('name * 'c) list> = 
        forM xs (fun (name, x) -> evalTemplate path (template x) >>=  fun a -> mreturn (name, a))

    let private applyFlocTemplate (xs: (FuncLocPath * 'a) list ) 
                        (template: 'a -> Template.Template<'b>) : CompilerMonad<'b list> = 
        forM xs (fun (path, x) -> evalTemplate path (template x))





    type EquipmentNumber = string

    type ClassEquiWorkList<'hole> = (EquipmentNumber * 'hole) list

    /// Generate Class and Char patches to update existing equipment.
    let compileClassEquiValuaEquiPatches (outputDirectory : string)
                                            (level : int)
                                            (filePrefix : string)
                                            (template : Class1<'hole>)
                                            (worklist : ClassEquiWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist1 = applyTemplate (FuncLocPath.Create("*****")) worklist template
            let! results = 
                forM worklist1 (fun (number, klass) -> equipmentToEquiProperties number [klass])
                    |>> collectClassEquiInstances
            do! writeEquiProperties outputDirectory level filePrefix results
            return ()
        }


    type ClassFlocWorkList<'hole> = (FuncLocPath * 'hole) list


    /// Generate Class and Char patches to update existing equipment.
    let compileClassFlocValuaFlocPatches (outputDirectory : string)
                                            (level : int)
                                            (filePrefix : string)
                                            (template : Class1<'hole>)
                                            (worklist : ClassFlocWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist1 = applyTemplate (FuncLocPath.Create("*****")) worklist template
            let! results = 
                forM worklist1 (fun (path, klass) -> funclocToClassFlocInstances path [klass])
                    |>> collectClassFlocInstances
            do! writeFlocProperties outputDirectory level filePrefix results
            return ()
        }


    // ************************************************************************
    // Compile hierarchy patches...


    // ************************************************************************
    // Components



    let private recWriteComponents (outputDirectory : string)
                                        (filePrefix : string)
                                        (worklist : S4Component list) : CompilerMonad<unit> = 
        compile {
            let! (fresults, eresults) = componentsEmit worklist
            do! writeFlocResults outputDirectory 8 filePrefix fresults
            do! writeEquiResults outputDirectory 8 filePrefix eresults
            return ()
        } 

    type ComponentWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 8 components and their subordinate equipment
    let compileComponentPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Component1<'hole>)
                                (worklist : ComponentWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! recWriteComponents outputDirectory filePrefix worklist
            return ()
        }                                                

    // ************************************************************************
    // Items


    let private recWriteItems (outputDirectory : string)
                                    (filePrefix : string)
                                    (worklist : S4Item list) : CompilerMonad<unit> = 
        compile {
            let! (fresults, eresults) = itemsEmit worklist
            do! writeFlocResults outputDirectory 7 filePrefix fresults
            do! writeEquiResults outputDirectory 7 filePrefix eresults
            let components = worklist |> List.map (fun x -> x.Components) |> List.concat
            do! recWriteComponents outputDirectory filePrefix components
            return ()
        } 

    type ItemWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 7 items and their subordinates
    let compileItemPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Item1<'hole>)
                                (worklist : ItemWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! recWriteItems outputDirectory filePrefix worklist
            return ()
        }  

    // ************************************************************************
    // Assembly


    let private recWriteAssemblies (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4Assembly list) : CompilerMonad<unit> = 
        compile {
            let! (fresults, eresults) = assembliesEmit worklist
            do! writeFlocResults outputDirectory 6 filePrefix fresults
            do! writeEquiResults outputDirectory 6 filePrefix eresults
            let items = worklist |> List.map (fun x -> x.Items) |> List.concat
            do! recWriteItems outputDirectory filePrefix items
            return ()
        } 

    type AssemblyWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 6 assemblies and their subordinates
    let compileAssemblyPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Assembly1<'hole>)
                                (worklist : AssemblyWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! recWriteAssemblies outputDirectory filePrefix worklist
            return ()
        }  

    
    // ************************************************************************
    // System


    let private recWriteSystems (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4System list) : CompilerMonad<unit> = 
        compile {
            let! (fresults, eresults) = systemsEmit worklist
            do! writeFlocResults outputDirectory 5 filePrefix fresults
            do! writeEquiResults outputDirectory 5 filePrefix eresults            
            let assemblies = worklist |> List.map (fun x -> x.Assemblies) |> List.concat
            do! recWriteAssemblies outputDirectory filePrefix assemblies
            return ()
        } 

    type SystemWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 5 systems and their subordinates
    let compileSystemPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : System1<'hole>)
                                (worklist : SystemWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! recWriteSystems outputDirectory filePrefix worklist
            return ()
        }
    
    
    // ************************************************************************
    // Process


    let private recWriteProcesses (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4Process list) : CompilerMonad<unit> = 
        compile {
            let! fresults = processesEmit worklist
            do! writeFlocResults outputDirectory 4 filePrefix fresults            
            let systems = worklist |> List.map (fun x -> x.Systems) |> List.concat
            do! recWriteSystems outputDirectory filePrefix systems
            return ()
        } 

    type ProcessWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 4 processes and their subordinates
    let compileProcessPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Process1<'hole>)
                                (worklist : ProcessWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! recWriteProcesses outputDirectory filePrefix worklist
            return ()
        }


    // ************************************************************************
    // ProcessGroups


    let private recWriteProcessGroups (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4ProcessGroup list) : CompilerMonad<unit> = 
        compile {
            let! fresults = processGroupsEmit worklist
            do! writeFlocResults outputDirectory 3 filePrefix fresults
            let processes = worklist |> List.map (fun x -> x.Processes) |> List.concat
            do! recWriteProcesses outputDirectory filePrefix processes
            return ()
        } 

    type ProcessGroupWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 3 process groups and their subordinates
    let compileProcessGroupPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : ProcessGroup1<'hole>)
                                (worklist : ProcessGroupWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! recWriteProcessGroups outputDirectory filePrefix worklist
            return ()
        }

    // ************************************************************************
    // Functions


    let private recWriteFunctions (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4Function list) : CompilerMonad<unit> = 
        compile {
            let! fresults = functionsEmit worklist
            do! writeFlocResults outputDirectory 2 filePrefix fresults
            let processGroups = worklist |> List.map (fun x -> x.ProcessGroups) |> List.concat
            do! recWriteProcessGroups outputDirectory filePrefix processGroups
            return ()
        } 

    type FunctionWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 2 functions and their subordinates
    let compileFunctionPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Function1<'hole>)
                                (worklist : FunctionWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! recWriteFunctions outputDirectory filePrefix worklist
            return ()
        }

    // ************************************************************************
    // Sites


    let private recWriteSites (outputDirectory : string)
                            (filePrefix : string)
                            (worklist : S4Site list) : CompilerMonad<unit> = 
        compile {
            let! fresults = sitesEmit worklist
            do! writeFlocResults outputDirectory 1 filePrefix fresults
            let functions = worklist |> List.map (fun x -> x.Functions) |> List.concat
            do! recWriteFunctions outputDirectory filePrefix functions
            return ()
        } 

    type SiteWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 1 sites and their subordinates
    let compileSitePatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Site1<'hole>)
                                (worklist : SiteWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! recWriteSites outputDirectory filePrefix worklist
            return ()
        }

    // ************************************************************************
    // EquiIndexing...


    /// Generate patches for a new level 1 site and its subordinates
    let materializeEquiClassValuaPatches (outputDirectory : string) : CompilerMonad<unit> = 
        let xlsxFile = Path.Combine(outputDirectory, "EquiIndexing.xlsx")
        if File.Exists(xlsxFile) then
            compile {            
                let! substs = readEquiIndexingSheet xlsxFile
                let sources = Directory.GetFiles(path = outputDirectory, searchPattern = "*.apch") |> List.ofArray
                do! mapMz (materializeEquiFile substs) sources
                return ()
            }
        else mreturn ()
