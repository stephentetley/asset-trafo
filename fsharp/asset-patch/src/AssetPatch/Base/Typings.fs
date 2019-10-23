// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Typings =

    open AssetPatch.Base.Syntax
    open AssetPatch.Base.Parser

    
    // ************************************************************************
    // Typing PatchFiles / Phantom types



    type FuncLocPhantom = class end
    
    type FuncLocPatch = PatchFile<FuncLocPhantom>

    
    let readFuncLocPatch (inputFile : string) : Result<FuncLocPatch, string> = 
        readPatch inputFile


    
    type ClassFlocPhantom = class end

    type ClassFlocPatch = PatchFile<ClassFlocPhantom>

    let readClassFlocPatch (inputFile : string) : Result<ClassFlocPatch, string> = 
        readPatch inputFile


    type ValuaFlocPhantom = class end
    
    type ValuaFlocPatch = PatchFile<ValuaFlocPhantom>

    let readValuaFlocPatch (inputFile : string) : Result<ValuaFlocPatch, string> = 
        readPatch inputFile



    type EquiPhantom = class end

    type EquiPatch = PatchFile<EquiPhantom>
    
    let readEquiPatch (inputFile : string) : Result<EquiPatch, string> = 
        readPatch inputFile
    
    type ClassEquiPhantom = class end

    type ClassEquiPatch = PatchFile<ClassEquiPhantom>
    
    let readClassEquiPatch (inputFile : string) : Result<ClassEquiPatch, string> = 
        readPatch inputFile
    

    type ValuaEquiPhantom = class end

    type ValuaEquiPatch = PatchFile<ValuaEquiPhantom>
    
    let readValuaEquiPatch (inputFile : string) : Result<ValuaEquiPatch, string> = 
        readPatch inputFile


