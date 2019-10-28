// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Typings =

    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.Parser

    
    // ************************************************************************
    // Typing PatchFiles / Phantom types

    // Discriminate the two categories of Selector Ids
    type FuncLocSelectors = interface end
    type EquiSelectors = interface end

    type FuncLocPhantom = 
        inherit FuncLocSelectors
    
    type FuncLocChangeFile = ChangeFile<FuncLocPhantom>

    
    let readFuncLocChangeFile (inputFile : string) : Result<FuncLocChangeFile, string> = 
        readChangeFile inputFile


    
    type ClassFlocPhantom =
        inherit FuncLocSelectors

    type ClassFlocChangeFile = ChangeFile<ClassFlocPhantom>

    let readClassFlocChangeFile (inputFile : string) : Result<ClassFlocChangeFile, string> = 
        readChangeFile inputFile


    type ValuaFlocPhantom = 
        inherit FuncLocSelectors
    
    type ValuaFlocChangeFile = ChangeFile<ValuaFlocPhantom>

    let readValuaFlocChangeFile (inputFile : string) : Result<ValuaFlocChangeFile, string> = 
        readChangeFile inputFile



    type EquiPhantom = 
        inherit EquiSelectors

    type EquiChangeFile = ChangeFile<EquiPhantom>
    
    let readEquiChangeFile (inputFile : string) : Result<EquiChangeFile, string> = 
        readChangeFile inputFile
    
    type ClassEquiPhantom = 
        inherit EquiSelectors

    type ClassEquiChangeFile = ChangeFile<ClassEquiPhantom>
    
    let readClassEquiChangeFile (inputFile : string) : Result<ClassEquiChangeFile, string> = 
        readChangeFile inputFile
    

    type ValuaEquiPhantom =
        inherit EquiSelectors

    type ValuaEquiChangeFile = ChangeFile<ValuaEquiPhantom>
    
    let readValuaEquiChangeFile (inputFile : string) : Result<ValuaEquiChangeFile, string> = 
        readChangeFile inputFile


    

