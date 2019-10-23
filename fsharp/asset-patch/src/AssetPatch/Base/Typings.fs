// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Typings =

    open AssetPatch.Base.Syntax
    open AssetPatch.Base.Parser

    
    // ************************************************************************
    // Typing PatchFiles / Phantom types

    // Discriminate the two categories of Selector Ids
    type FuncLocSelectors = interface end
    type EquiSelectors = interface end

    type FuncLocPhantom = 
        inherit FuncLocSelectors
    
    type FuncLocPatch = PatchFile<FuncLocPhantom>

    
    let readFuncLocPatch (inputFile : string) : Result<FuncLocPatch, string> = 
        readPatch inputFile


    
    type ClassFlocPhantom =
        inherit FuncLocSelectors

    type ClassFlocPatch = PatchFile<ClassFlocPhantom>

    let readClassFlocPatch (inputFile : string) : Result<ClassFlocPatch, string> = 
        readPatch inputFile


    type ValuaFlocPhantom = 
        inherit FuncLocSelectors
    
    type ValuaFlocPatch = PatchFile<ValuaFlocPhantom>

    let readValuaFlocPatch (inputFile : string) : Result<ValuaFlocPatch, string> = 
        readPatch inputFile



    type EquiPhantom = 
        inherit EquiSelectors

    type EquiPatch = PatchFile<EquiPhantom>
    
    let readEquiPatch (inputFile : string) : Result<EquiPatch, string> = 
        readPatch inputFile
    
    type ClassEquiPhantom = 
        inherit EquiSelectors

    type ClassEquiPatch = PatchFile<ClassEquiPhantom>
    
    let readClassEquiPatch (inputFile : string) : Result<ClassEquiPatch, string> = 
        readPatch inputFile
    

    type ValuaEquiPhantom =
        inherit EquiSelectors

    type ValuaEquiPatch = PatchFile<ValuaEquiPhantom>
    
    let readValuaEquiPatch (inputFile : string) : Result<ValuaEquiPatch, string> = 
        readPatch inputFile


    

