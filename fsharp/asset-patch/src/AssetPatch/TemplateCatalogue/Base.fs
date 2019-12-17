﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplateCatalogue


[<AutoOpen>]
module Base =
    
    open AssetPatch.TemplatePatcher.CommonTypes
    open AssetPatch.TemplatePatcher.Template

    // ************************************************************************
    // Function

    let _no_functions_ : Function list = []

    let control_and_automation : Class list -> ProcessGroup list -> Function =
        _function "CAA" "Control and Automation" "CAA"

    let electrical_power_supply : Class list -> ProcessGroup list -> Function =
        _function "E" "Electrical Power Supply" "EPS"

    let environmental_discharge : Class list -> ProcessGroup list -> Function =
        _function "EDC" "Environmental Discharge" "EDC"

    let site_infrastructure : Class list -> ProcessGroup list -> Function =
        _function "SIF" "Site Infrastructure" "SIF"


    // ************************************************************************
    // Process Group

    let _no_process_groups_ : ProcessGroup list = []

    let control : Class list -> Process list -> ProcessGroup =
        _processGroup "CON" "Control" "CON"

    let combined_treatment : Class list -> Process list -> ProcessGroup =
        _processGroup "COT" "Combined Treatment" "COT"

    let liquid_discharge : Class list -> Process list -> ProcessGroup =
        _processGroup "LQD" "Liquid Discharge" "LQD"

    let networks : Class list -> Process list -> ProcessGroup =
        _processGroup "NET" "Networks" "NET"

    let waste_water_transfer : Class list -> Process list -> ProcessGroup =
        _processGroup "WTF" "Waste Water Transfer" "WTF"


    // ************************************************************************
    // Process

    let _no_processes_ : Process list = []

    let pumping : Class list -> System list -> Process =
        _process "PMG" "Pumping" "PMG"

    let regulatory_monitoring : Class list -> System list -> Process =
        _process "RGM" "Regulatory Monitoring" "RGM"

    let telemetry : Class list -> System list -> Process =
        _process "TEL" "Telemetry" "TEL"

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
    // Assembly

    let _no_assemblies_ : Assembly list = []
    
    // ************************************************************************
    // Item

    let _no_items_ : Item list = []

    // ************************************************************************
    // Component

    let _no_components_ : Component list = []

    
    // ************************************************************************
    // Equipment

    let _no_equipment_ : Equipment list = []
    let _no_subordinate_equipment_ : Equipment list = []

    let lstn_level_transmitter (name : string) 
                : Class list -> Equipment list -> EquipmentAttribute list -> Equipment = 
        _equipment name "I" "LSTN"

    let telemetry_outstation (name : string) 
                : Class list -> Equipment list -> EquipmentAttribute list -> Equipment = 
        _equipment name "I" "NETW"

    // ************************************************************************
    // Equipment attributes

    let manufacturer (name : string) : EquipmentAttribute = 
        equipmentAttribute <| fun e1 ->  { e1 with Manufacturer = Some name }
        

    let model (name : string) : EquipmentAttribute =
        equipmentAttribute <| fun e1 -> { e1 with Model = Some name }
        
    let serial_number (productCode : string) : EquipmentAttribute =
        equipmentAttribute <| fun e1 -> { e1 with SerialNumber = Some productCode }


    let construction_year (year : uint16) : EquipmentAttribute = 
        equipmentAttribute <| fun e1 -> { e1 with ConstructionYear = Some year }
    

    let construction_month (month : uint8) : EquipmentAttribute = 
        equipmentAttribute <| fun e1 -> { e1 with ConstructionMonth = Some month }
    
    // ************************************************************************
    // Classes and characteritics

    /// *:UNICLASS_CODE
    /// This is currently blank
    let uniclass_code () : Characteristic = 
        _characteristic "UNICLASS_CODE" NullValue

    /// *:UNICLASS_DESC
    /// This is currently blank
    let uniclass_desc () : Characteristic = 
        _characteristic "UNICLASS_DESC" NullValue


    /// AIB_REFERENCE
    let aib_reference : Characteristic list -> Class = 
        _class "AIB_REFERENCE"

    /// AIB_REFERENCE:AI2_AIB_REFERENCE
    let ai2_aib_reference (v : string) : Characteristic = 
        _characteristic "AI2_AIB_REFERENCE" (TextValue v)

    /// AIB_REFERENCE:S4_AIB_REFERENCE
    /// This is always blank
    let s4_aib_reference () : Characteristic = 
        _characteristic "S4_AIB_REFERENCE" NullValue


    

    /// Class:EAST_NORTH
    let east_north : Characteristic list -> Class = 
        _class "EAST_NORTH" 

    /// EAST_NORTH:EASTING
    let easting (v : int) : Characteristic = 
        _characteristic "EASTING" (intValue v)

    /// EAST_NORTH:NORTHING
    let northing (v : int) : Characteristic = 
        _characteristic "NORTHING" (intValue v)

    












    

    



   