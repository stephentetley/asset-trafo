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


    let _no_equipment_ : Equipment list = []

    let lstn_level_transmitter (name : string) : Class list -> Equipment = 
        _equipment name "LSTN"

    let telemetry_outstation (name : string) : Class list -> Equipment = 
        _equipment name "NETW"


    // ************************************************************************
    // Component

    let _no_components_ : Component list = []


    // ************************************************************************
    // Item

    let _no_items_ : Item list = []

    // ************************************************************************
    // Assembly

    let _no_assemblies_ : Assembly list = []


    // ************************************************************************
    // System

    let _no_systems_ : System list = []

    let montoring_system (shortCode : string) (description : string) 
                : Class list -> Assembly list -> Equipment list -> System =        
        _system shortCode description "SMON"


    let telemetry_system (shortCode : string) (description : string)  
                : Class list -> Assembly list -> Equipment list -> System =    
        _system shortCode description "CTOS"

    // ************************************************************************
    // Process

    let _no_processes_ : Process list = []

    let regulatory_monitoring : Class list -> System list -> Process =
        _process "RGM" "Regulatory Monitoring" "RGM"

    let telemetry : Class list -> System list -> Process =
        _process "TEL" "Telemetry" "TEL"

    // ************************************************************************
    // Process Group

    let _no_process_groups_ : ProcessGroup list = []

    let liquid_discharge : Class list -> Process list -> ProcessGroup =
        _processGroup "LQD" "Liquid Discharge" "LQD"

    let networks : Class list -> Process list -> ProcessGroup =
        _processGroup "NET" "Networks" "NET"

    // ************************************************************************
    // Function

    let _no_functions_ : Function list = []

    let environmental_discharge : Class list -> ProcessGroup list -> Function =
        _function "EDC" "Environmental Discharge" "EDC"

    let control_automation : Class list -> ProcessGroup list -> Function =
        _function "CAA" "Control and Automation" "CAA"