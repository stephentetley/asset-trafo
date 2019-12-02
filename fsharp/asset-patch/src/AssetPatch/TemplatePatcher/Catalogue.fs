// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module Catalogue =
    
    open AssetPatch.TemplatePatcher.Template

    
    let manufacturer (name : string) : EquipmentAttribute = 
        equipmentAttribute <| fun e1 ->  { e1 with Manufacturer = Some name }
        

    let model (name : string) : EquipmentAttribute =
        equipmentAttribute <| fun e1 -> { e1 with Model = Some name }
        
    let serial_number (productCode : string) : EquipmentAttribute =
        equipmentAttribute <| fun e1 -> { e1 with SerialNumber = Some productCode }



    
    // ************************************************************************
    // Classes and characteritics

    /// AIB_REFERENCE
    let aib_reference : Characteristic list -> Class = 
        _class "AIB_REFERENCE" 850u

    /// AIB_REFERENCE:AI2_AIB_REFERENCE
    let ai2_aib_reference (v : string) : Characteristic = 
        _characteristic "AI2_AIB_REFERENCE" v

    /// AIB_REFERENCE:S4_AIB_REFERENCE
    /// This is always blank
    let s4_aib_reference () : Characteristic = 
        _characteristic "S4_AIB_REFERENCE" ""


    /// ASSET_CONDITION
    let asset_condition : Characteristic list -> Class = 
        _class "ASSET_CONDITION" 1013u

    // ASSET_CONDITION:ConditionGrade
    type ConditionGrade = 
        | Good 

        override x.ToString() = 
            match x with
            | Good -> "1 - GOOD"

            
    /// ASSET_CONDITION:CONDITION_GRADE
    let condition_grade (v : ConditionGrade) : Characteristic = 
        _characteristic "CONDITION_GRADE" (v.ToString())


    /// ASSET_CONDITION:CONDITION_GRADE_REASON
    let condition_grade_reason (v : string) : Characteristic = 
        _characteristic "CONDITION_GRADE_REASON" (v.ToUpper())

    /// ASSET_CONDITION:PerformanceGrade
    type PerformanceGrade = 
        | Availability_95 

        override x.ToString() = 
            match x with
            | Availability_95 -> "1 - AVAILABILITY 95%"


    /// ASSET_CONDITION:PERFORMANCE_GRADE
    let performance_grade (v : PerformanceGrade) : Characteristic = 
        _characteristic "PERFORMANCE_GRADE" (v.ToString())


    /// ASSET_CONDITION:PERFORMANCE_GRADE_REASON
    let performance_grade_reason (v : string) : Characteristic = 
        _characteristic "PERFORMANCE_GRADE_REASON" (v.ToUpper())


    /// ASSET_CONDITION:LoadingFactor
    type LoadingFactor = 
        | Satisfactory 

        override x.ToString() = 
            match x with
            | Satisfactory -> "3 - SATISFACTORY"

    /// ASSET_CONDITION:LOADING_FACTOR
    let loading_factor (v : LoadingFactor) : Characteristic = 
        _characteristic "LOADING_FACTOR" (v.ToString())


    /// ASSET_CONDITION:LOADING_FACTOR_REASON
    let loading_factor_reason (v : string) : Characteristic = 
         _characteristic "LOADING_FACTOR_REASON" (v.ToUpper())


    /// ASSET_CONDITION:SURVEY_DATE
    let survey_date (v : uint32) : Characteristic = 
        _characteristic "SURVEY_DATE" (v.ToString())


    /// EAST_NORTH
    let east_north : Characteristic list -> Class = 
        _class "EAST_NORTH" 379u 

    /// EAST_NORTH:EASTING
    let easting (v : int) : Characteristic = 
        _characteristic "EASTING" (v.ToString())

    /// EAST_NORTH:NORTHING
    let northing (v : int) : Characteristic = 
        _characteristic "NORTHING" (v.ToString())


    
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