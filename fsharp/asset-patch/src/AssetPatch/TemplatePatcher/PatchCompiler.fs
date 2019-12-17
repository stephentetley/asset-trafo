// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module PatchCompiler =
    
    open System.IO

    // Open first so common names get overridden
    open AssetPatch.TemplatePatcher.Template

    open AssetPatch.Base.FuncLocPath 
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.CompilerMonad
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



    // ************************************************************************
    // Compile hierarchy patches...


    // ************************************************************************
    // Components



    let private writeComponentsPhase1 (outputDirectory : string)
                                         (filePrefix : string)
                                         (worklist : S4Component list) : CompilerMonad<unit> = 
        compile {
            let! ans = componentsEmitPhase1 worklist
            do! writePhase1FlocData outputDirectory filePrefix (ans.FlocData.RemoveDups())
            do! writePhase1EquiData outputDirectory filePrefix ans.EquiData
            return ()
        } 

    let private writeComponentsPhase2 (outputDirectory : string)
                                        (filePrefix : string)
                                        (worklist : S4Component list) : CompilerMonad<unit> = 
        compile {
            let! ans = componentsEmitPhase2 worklist
            do! writePhase2EquiData outputDirectory filePrefix ans
            return ()
        } 

    type ComponentWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 8 components and their subordinate equipment
    let compileComponentsPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Component1<'hole>)
                                (worklist : ComponentWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeComponentsPhase1 outputDirectory filePrefix worklist
            return ()
        }       
    
    /// Generate patches for level 8 component equipment - classes and characteristics
    let compileComponentsPhase2 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Component1<'hole>)
                                (worklist : ComponentWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeComponentsPhase1 outputDirectory filePrefix worklist
            return ()
        } 

    // ************************************************************************
    // Items


    let private writeItemsPhase1 (outputDirectory : string)
                                    (filePrefix : string)
                                    (worklist : S4Item list) : CompilerMonad<unit> = 
        compile {
            let! ans = itemsEmitPhase1 worklist
            do! writePhase1FlocData outputDirectory filePrefix (ans.FlocData.RemoveDups())
            do! writePhase1EquiData outputDirectory filePrefix ans.EquiData
            return ()
        } 

    let private writeItemsPhase2 (outputDirectory : string)
                                    (filePrefix : string)
                                    (worklist : S4Item list) : CompilerMonad<unit> = 
        compile {
            let! ans = itemsEmitPhase2 worklist
            do! writePhase2EquiData outputDirectory filePrefix ans
            return ()
        } 

    type ItemWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 7 items and their subordinates
    let compileItemsPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Item1<'hole>)
                                (worklist : ItemWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeItemsPhase1 outputDirectory filePrefix worklist
            return ()
        }  

    /// Generate patches for level 7 item equipment - classes and characteristics
    let compileItemsPhase2 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Item1<'hole>)
                                (worklist : ItemWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeItemsPhase2 outputDirectory filePrefix worklist
            return ()
        } 

    // ************************************************************************
    // Assembly


    let private writeAssembliesPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4Assembly list) : CompilerMonad<unit> = 
        compile {
            let! ans = assembliesEmitPhase1 worklist
            do! writePhase1FlocData outputDirectory filePrefix (ans.FlocData.RemoveDups())
            do! writePhase1EquiData outputDirectory filePrefix ans.EquiData
            return ()
        
        } 

    let private writeAssembliesPhase2 (outputDirectory : string)
                                        (filePrefix : string)
                                        (worklist : S4Assembly list) : CompilerMonad<unit> = 
        compile {
            let! ans = assembliesEmitPhase2 worklist
            do! writePhase2EquiData outputDirectory filePrefix ans
            return ()
        
        } 

    type AssemblyWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 6 assemblies and their subordinates
    let compileAssembliesPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Assembly1<'hole>)
                                (worklist : AssemblyWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeAssembliesPhase1 outputDirectory filePrefix worklist
            return ()
        }  

    /// Generate patches for level 6 assembly equipment - classes and characteristics
    let compileAssembliesPhase2 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Assembly1<'hole>)
                                (worklist : AssemblyWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeAssembliesPhase2 outputDirectory filePrefix worklist
            return ()
        } 
    
    // ************************************************************************
    // System


    let private writeSystemsPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4System list) : CompilerMonad<unit> = 
        compile {
            let! ans = systemsEmitPhase1 worklist
            do! writePhase1FlocData outputDirectory filePrefix (ans.FlocData.RemoveDups())
            do! writePhase1EquiData outputDirectory filePrefix ans.EquiData
            return ()
        }

    let private writeSystemsPhase2 (outputDirectory : string)
                                    (filePrefix : string)
                                    (worklist : S4System list) : CompilerMonad<unit> = 
        compile {
            let! ans = systemsEmitPhase2 worklist
            do! writePhase2EquiData outputDirectory filePrefix ans
            return ()
        }

    type SystemWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 5 systems and their subordinates
    let compileSystemsPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (template : System1<'hole>)
                                (worklist : SystemWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeSystemsPhase1 outputDirectory filePrefix worklist
            return ()
        }
    
    /// Generate patches for level 5 system equipment - classes and characteristics    
    let compileSystemsPhase2 (outputDirectory : string)
                                (filePrefix : string)
                                (template : System1<'hole>)
                                (worklist : SystemWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeSystemsPhase2 outputDirectory filePrefix worklist
            return ()
        }
    // ************************************************************************
    // Process


    let private writeProcessesPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4Process list) : CompilerMonad<unit> = 
        compile {
            let! ans = processesEmitPhase1 worklist
            do! writePhase1FlocData outputDirectory filePrefix (ans.FlocData.RemoveDups())
            do! writePhase1EquiData outputDirectory filePrefix ans.EquiData
            return ()
        } 

    let private writeProcessesPhase2 (outputDirectory : string)
                                        (filePrefix : string)
                                        (worklist : S4Process list) : CompilerMonad<unit> = 
        compile {
            let! ans = processesEmitPhase2 worklist
            do! writePhase2EquiData outputDirectory filePrefix ans
            return ()
        }

    type ProcessWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 4 processes and their subordinates
    let compileProcessesPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Process1<'hole>)
                                (worklist : ProcessWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeProcessesPhase1 outputDirectory filePrefix worklist
            return ()
        }

    /// Generate patches for descendant equipment of level 4 processes - classes and characteristics
    let compileProcessesPhase2 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Process1<'hole>)
                                (worklist : ProcessWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeProcessesPhase2 outputDirectory filePrefix worklist
            return ()
        }

    // ************************************************************************
    // ProcessGroups


    let private writeProcessGroupsPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4ProcessGroup list) : CompilerMonad<unit> = 
        compile {
            let! ans = processGroupsEmitPhase1 worklist
            do! writePhase1FlocData outputDirectory filePrefix (ans.FlocData.RemoveDups())
            do! writePhase1EquiData outputDirectory filePrefix ans.EquiData
            return ()
        }

    let private writeProcessGroupsPhase2 (outputDirectory : string)
                                            (filePrefix : string)
                                            (worklist : S4ProcessGroup list) : CompilerMonad<unit> = 
        compile {
            let! ans = processGroupsEmitPhase2 worklist
            do! writePhase2EquiData outputDirectory filePrefix ans
            return ()
        }

    type ProcessGroupWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 3 process groups and their subordinates
    let compileProcessGroupsPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (template : ProcessGroup1<'hole>)
                                (worklist : ProcessGroupWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeProcessGroupsPhase1 outputDirectory filePrefix worklist
            return ()
        }

    /// Generate patches for descendant equipment of level 3 process groups - classes and characteristics
    let compileProcessGroupsPhase2 (outputDirectory : string)
                                    (filePrefix : string)
                                    (template : ProcessGroup1<'hole>)
                                    (worklist : ProcessGroupWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeProcessGroupsPhase2 outputDirectory filePrefix worklist
            return ()
        }

    // ************************************************************************
    // Functions


    let private writeFunctionsPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (worklist : S4Function list) : CompilerMonad<unit> = 
        compile {
            let! ans = functionsEmitPhase1 worklist
            do! writePhase1FlocData outputDirectory filePrefix (ans.FlocData.RemoveDups())
            do! writePhase1EquiData outputDirectory filePrefix ans.EquiData
            return ()
        } 

    let private writeFunctionsPhase2 (outputDirectory : string)
                                        (filePrefix : string)
                                        (worklist : S4Function list) : CompilerMonad<unit> = 
        compile {
            let! ans = functionsEmitPhase2 worklist
            do! writePhase2EquiData outputDirectory filePrefix ans
            return ()
        } 

    type FunctionWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 2 functions and their subordinates
    let compileFunctionPatchesPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Function1<'hole>)
                                (worklist : FunctionWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeFunctionsPhase1 outputDirectory filePrefix worklist
            return ()
        }

    /// Generate patches for descendant equipment of level 2 functions - classes and characteristics
    let compileFunctionPatchesPhase2 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Function1<'hole>)
                                (worklist : FunctionWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeFunctionsPhase2 outputDirectory filePrefix worklist
            return ()
        }

    // ************************************************************************
    // Sites


    let private writeSitesPhase1 (outputDirectory : string)
                            (filePrefix : string)
                            (worklist : S4Site list) : CompilerMonad<unit> = 
        compile {
            let! ans = sitesEmitPhase1 worklist
            do! writePhase1FlocData outputDirectory filePrefix (ans.FlocData.RemoveDups())
            do! writePhase1EquiData outputDirectory filePrefix ans.EquiData
            return ()
        } 

    let private writeSitesPhase2 (outputDirectory : string)
                                    (filePrefix : string)
                                    (worklist : S4Site list) : CompilerMonad<unit> = 
        compile {
            let! ans = sitesEmitPhase2 worklist
            do! writePhase2EquiData outputDirectory filePrefix ans
            return ()
        }

    type SiteWorkList<'hole> = (FuncLocPath * 'hole) list

    /// Generate patches for new level 1 sites and their subordinates
    let compileSitePatchesPhase1 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Site1<'hole>)
                                (worklist : SiteWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeSitesPhase1 outputDirectory filePrefix worklist
            return ()
        }

    /// Generate patches for descendant equipment of level 1 sites - classes and characteristics
    let compileSitePatchesPhase2 (outputDirectory : string)
                                (filePrefix : string)
                                (template : Site1<'hole>)
                                (worklist : SiteWorkList<'hole>) : CompilerMonad<unit> = 
        compile {
            let! worklist = applyFlocTemplate worklist template
            do! writeSitesPhase2 outputDirectory filePrefix worklist
            return ()
        }