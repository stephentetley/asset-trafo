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
    open AssetPatch.TemplatePatcher.EquiIndexing
    open AssetPatch.TemplatePatcher.EmitEquipment
    open AssetPatch.TemplatePatcher.EmitFuncLoc
    open AssetPatch.TemplatePatcher.Emitter
    


    // ************************************************************************
    // Compile Class * Valua patches to update exting Flocs and Equipment...


    let private applyTemplate (xs: ('name * 'b) list ) 
                        (template: 'b -> Template.Template<'c>) : CompilerMonad<('name * 'c) list> = 
        forM xs (fun (name, x) -> cmEvalTemplate (template x) >>=  fun a -> mreturn (name, a))

    
    type EquipmentNumber = string

    type ClassEquiWorkList<'hole> = (EquipmentNumber * 'hole) list

    /// Generate Class and Char patches to update existing equipment.
    let compileClassEquiValuaEquiPatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (template : Class1<'hole>)
                                         (worklist : ClassEquiWorkList<'hole>)
                                            : CompilerMonad<unit> = 
        compile {
            let! worklist1 = applyTemplate worklist template
            let! results = 
                forM worklist1 (fun (number, klass) -> equipmentToEquiProperties number [klass])
                    |>> collectEquiProperties
            do! writeEquiProperties outputDirectory filePrefix results
            return ()
        }


    type ClassFlocWorkList<'hole> = (FuncLocPath * 'hole) list


    /// Generate Class and Char patches to update existing equipment.
    let compileClassFlocValuaFlocPatches (outputDirectory : string)
                                         (filePrefix : string)
                                         (template : Class1<'hole>)
                                         (worklist : ClassFlocWorkList<'hole>)
                                            : CompilerMonad<unit> = 
        compile {
            let! worklist1 = applyTemplate worklist template
            let! results = 
                forM worklist1 (fun (path, klass) -> funclocToFlocProperties path [klass])
                    |>> collectFlocProperties
            do! writeFlocProperties outputDirectory filePrefix results
            return ()
        }


    // ************************************************************************
    // Compile hierarchy patches...


    // ************************************************************************
    // Components



    let private writeComponentPatches (outputDirectory : string)
                                        (filePrefix : string)
                                        (worklist : S4Component list) : CompilerMonad<unit> = 
        compile {
            let! (fresults, eresults) = componentsEmit worklist
            do! writeFlocResults outputDirectory filePrefix fresults
            do! writeEquiResults outputDirectory filePrefix eresults
            return ()
        } 

    type ComponentWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 8 components and their subordinate equipment
    let compileComponentPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Component1<'hole>)
                                (worklist : ComponentWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyTemplate worklist template |>> List.map snd
            do! writeComponentPatches outputDirectory filePrefix worklist
            return ()
        }                                                

    // ************************************************************************
    // Items


    let private writeItemPatches (outputDirectory : string)
                                    (filePrefix : string)
                                    (worklist : S4Item list) : CompilerMonad<unit> = 
        compile {
            let! (fresults, eresults) = itemsEmit worklist
            do! writeFlocResults outputDirectory filePrefix fresults
            do! writeEquiResults outputDirectory filePrefix eresults
            return ()
        } 

    type ItemWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 7 items and their subordinates
    let compileItemPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Item1<'hole>)
                                (worklist : ItemWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyTemplate worklist template |>> List.map snd
            do! writeItemPatches outputDirectory filePrefix worklist
            let components = worklist |> List.map (fun x -> x.Components) |> List.concat
            do! writeComponentPatches outputDirectory filePrefix components
            return ()
        }  

    // ************************************************************************
    // Assembly


    let writeAssemblyPatches (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4Assembly list) : CompilerMonad<unit> = 
        compile {
            let! (fresults, eresults) = assembliesEmit worklist
            do! writeFlocResults outputDirectory filePrefix fresults
            do! writeEquiResults outputDirectory filePrefix eresults
            return ()
        } 

    type AssemblyWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 6 assemblies and their subordinates
    let compileAssemblyPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Assembly1<'hole>)
                                (worklist : AssemblyWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyTemplate worklist template |>> List.map snd
            do! writeAssemblyPatches outputDirectory filePrefix worklist
            let items = worklist |> List.map (fun x -> x.Items) |> List.concat
            do! writeItemPatches outputDirectory filePrefix items
            return ()
        }  

    
    // ************************************************************************
    // System


    let writeSystemPatches (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4System list) : CompilerMonad<unit> = 
        compile {
            let! (fresults, eresults) = systemsEmit worklist
            do! writeFlocResults outputDirectory filePrefix fresults
            do! writeEquiResults outputDirectory filePrefix eresults
            return ()
        } 

    type SystemWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 5 systems and their subordinates
    let compileSystemPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : System1<'hole>)
                                (worklist : SystemWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyTemplate worklist template |>> List.map snd
            do! writeSystemPatches outputDirectory filePrefix worklist
            let assemblies = worklist |> List.map (fun x -> x.Assemblies) |> List.concat
            do! writeAssemblyPatches outputDirectory filePrefix assemblies
            return ()
        }
    
    
    // ************************************************************************
    // Process


    let writeProcessPatches (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4Process list) : CompilerMonad<unit> = 
        compile {
            let! fresults = processesEmit worklist
            do! writeFlocResults outputDirectory filePrefix fresults
            return ()
        } 

    type ProcessWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 4 processes and their subordinates
    let compileProcessPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Process1<'hole>)
                                (worklist : ProcessWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyTemplate worklist template |>> List.map snd
            do! writeProcessPatches outputDirectory filePrefix worklist
            let systems = worklist |> List.map (fun x -> x.Systems) |> List.concat
            do! writeSystemPatches outputDirectory filePrefix systems
            return ()
        }


    // ************************************************************************
    // ProcessGroups


    let writeProcessGroupPatches (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4ProcessGroup list) : CompilerMonad<unit> = 
        compile {
            let! fresults = processGroupsEmit worklist
            do! writeFlocResults outputDirectory filePrefix fresults
            return ()
        } 

    type ProcessGroupWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 3 process groups and their subordinates
    let compileProcessGroupPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : ProcessGroup1<'hole>)
                                (worklist : ProcessGroupWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyTemplate worklist template |>> List.map snd
            do! writeProcessGroupPatches outputDirectory filePrefix worklist
            let processes = worklist |> List.map (fun x -> x.Processes) |> List.concat
            do! writeProcessPatches outputDirectory filePrefix processes
            return ()
        }

    // ************************************************************************
    // Functions


    let writeFunctionPatches (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4Function list) : CompilerMonad<unit> = 
        compile {
            let! fresults = functionsEmit worklist
            do! writeFlocResults outputDirectory filePrefix fresults
            return ()
        } 

    type FunctionWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 2 functions and their subordinates
    let compileFunctionPatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Function1<'hole>)
                                (worklist : FunctionWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyTemplate worklist template |>> List.map snd
            do! writeFunctionPatches outputDirectory filePrefix worklist
            let processGroups = worklist |> List.map (fun x -> x.ProcessGroups) |> List.concat
            do! writeProcessGroupPatches outputDirectory filePrefix processGroups
            return ()
        }

    // ************************************************************************
    // Sites


    let writeSitePatches (outputDirectory : string)
                            (filePrefix : string)
                            (worklist : S4Site list) : CompilerMonad<unit> = 
        compile {
            let! fresults = sitesEmit worklist
            do! writeFlocResults outputDirectory filePrefix fresults
            return ()
        } 

    type SiteWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 1 sites and their subordinates
    let compileSitePatches (outputDirectory : string)
                                (filePrefix : string)
                                (template : Site1<'hole>)
                                (worklist : SiteWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyTemplate worklist template |>> List.map snd
            do! writeSitePatches outputDirectory filePrefix worklist
            let functions = worklist |> List.map (fun x -> x.Functions) |> List.concat
            do! writeFunctionPatches outputDirectory filePrefix functions
            return ()
        }

    // ************************************************************************
    // EquiIndexing...


    /// Generate patches for a new level 1 site and its subordinates
    let materializeEquiClassValuaPatches (outputDirectory : string) : CompilerMonad<unit> = 
        compile {
            let xlsxFile = Path.Combine(outputDirectory, "EquiIndexing.xlsx")
            let! substs = readEquiIndexingSheet xlsxFile
            let sources = Directory.GetFiles(path = outputDirectory, searchPattern = "*.apch") |> List.ofArray
            do! mapMz (materializeEquiFile substs) sources
            return ()
        }