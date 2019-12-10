// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplateCatalogue


[<AutoOpen>]
module Lstnut =
    
    open AssetPatch.TemplatePatcher.Template

    
    

    /// Class:LSTNUT
    let lstnut : Characteristic list -> Class = 
        _class "LSTNUT"     

    /// LSTNUT:RelayFunction
    type RelayFunction = 
        | AlternateDutyAssist
        | HighLevelAlarm
        | LossOfEcho 

        override x.ToString() = 
            match x with
            | AlternateDutyAssist -> "ALTERNATE DUTY ASSIST"
            | HighLevelAlarm -> "HIGH LEVEL ALARM"
            | LossOfEcho -> "LOSS OF ECHO"

        static member TryParse (source : string) : RelayFunction option = 
            match source with
            | null | "" -> None
            | _ -> 
                match source.ToUpper() with
                | "ALTERNATE DUTY ASSIST" -> Some AlternateDutyAssist
                | "HIGH LEVEL ALARM" -> Some HighLevelAlarm
                | "LOSS OF ECHO" -> Some LossOfEcho 
                | _ -> None

    /// LSTN_RELAY_{ix}_FUNCTION
    let lstn_relay_function (ix : int) (v : RelayFunction) : Characteristic = 
        let name = sprintf "LSTN_RELAY_%i_FUNCTION" ix
        _characteristic name (v.ToString())


    /// LSTN_RELAY_{ix}_ON_LEVEL_M
    let lstn_relay_on_level (ix : int) (v : decimal) : Characteristic = 
        let name = sprintf "LSTN_RELAY_%i_ON_LEVEL_M" ix
        _characteristic name (sprintf "%M" v)

    /// LSTN_RELAY_{ix}_OFF_LEVEL_M
    let lstn_relay_off_level (ix : int) (v : decimal) : Characteristic = 
        let name = sprintf "LSTN_RELAY_%i_OFF_LEVEL_M" ix
        _characteristic name (sprintf "%M" v)

    /// LSTN_TRANSDUCER_MODEL
    let lstn_transducer_model (v : string) : Characteristic =         
        _characteristic "LSTN_TRANSDUCER_MODEL" v

    /// LSTN_TRANSDUCER_SERIAL_NO
    let lstn_transducer_serial_no (v : string) : Characteristic =         
        _characteristic "LSTN_TRANSDUCER_SERIAL_NO" v

    /// LSTN_RANGE_MIN
    let lstn_range_min (v : decimal) : Characteristic =         
        _characteristic "LSTN_RANGE_MIN" (sprintf "%M" v)

    /// LSTN_RANGE_MAX
    let lstn_range_max (v : decimal) : Characteristic =         
        _characteristic "LSTN_RANGE_MAX" (sprintf "%M" v)


    /// LSTN_RANGE_UNITS
    let lstn_range_units (v : string) : Characteristic =         
        _characteristic "LSTN_RANGE_UNITS" v

    /// LSTN_SIGNAL_TYPE
    let lstn_signal_type (v : string) : Characteristic =         
        _characteristic "LSTN_SIGNAL_TYPE" v

    /// LSTN_SUPPLY_VOLTAGE
    let lstn_supply_voltage (v : int) : Characteristic =         
        _characteristic "LSTN_SUPPLY_VOLTAGE" (v.ToString())

    /// IP_RATING
    let ip_rating (v : int) : Characteristic =         
        _characteristic "IP_RATING" (v.ToString())