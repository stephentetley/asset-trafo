// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocBuilder



module Catalogue =
    
    open AssetPatch.FlocBuilder.Hierarchy
    
    // ************************************************************************
    // Classes and characteritics

    /// AIB_REFERENCE
    let aib_reference : Characteristic list -> Class = 
        _class "AIB_REFERENCE" 850u

    /// AIB_REFERENCE:AI2_AIB_REFERENCE
    let ai2_aib_reference (v : string) : Characteristic = 
        _char "AI2_AIB_REFERENCE" v

    /// AIB_REFERENCE:S4_AIB_REFERENCE
    /// This is always blank
    let s4_aib_reference () : Characteristic = 
        _char "S4_AIB_REFERENCE" ""


    /// EAST_NORTH
    let east_north : Characteristic list -> Class = 
        _class "EAST_NORTH" 379u 

    /// EAST_NORTH:EASTING
    let easting (v : int) : Characteristic = 
        _char "EASTING" (v.ToString())

    /// EAST_NORTH:NORTHING
    let northing (v : int) : Characteristic = 
        _char "NORTHING" (v.ToString())


    
    // ************************************************************************
    // Equipment


    let lstn_level_transmitter (name : string) : Class list -> Equipment = 
        _equipment name "LSTN"


    // ************************************************************************
    // System



    let smon (shortCode : string) (description : string) 
                : Class list -> Assembly list -> Equipment list -> System =
        
        _system shortCode description "SMON"

    // ************************************************************************
    // Process

    let rgm : Class list -> System list -> Process =
        _process "RGM" "Regulatory Monitoring" "RGM"


    // ************************************************************************
    // Process Group

    let lqd : Class list -> Process list -> ProcessGroup =
        _processGroup "LQD" "Liquid Discharge" "LQD"

    // ************************************************************************
    // Function

    let edc : Class list -> ProcessGroup list -> Function =
        _function "EDC" "Environmental Discharge" "EDC"

