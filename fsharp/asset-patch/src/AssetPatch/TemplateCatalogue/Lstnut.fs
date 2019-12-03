// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplateCatalogue


[<AutoOpen>]
module Lstnut =
    
    open AssetPatch.TemplatePatcher.Template

    
    

    /// Class:LSTNUT
    let lstnut : Characteristic list -> Class = 
        _class "LSTNUT" 973u 
    

    /// LSTNUT:RelayFunction
    type RelayFunction = 
        | AlternateDutyAssist
        | LossOfEcho 

        override x.ToString() = 
            match x with
            | AlternateDutyAssist -> "ALTERNATE DUTY ASSIST"
            | LossOfEcho -> "LOSS OF ECHO"

        static member TryParse (source : string) : RelayFunction option = 
            match source.ToUpper() with
            | "ALTERNATE DUTY ASSIST" -> Some AlternateDutyAssist
            | "LOSS OF ECHO" -> Some LossOfEcho 
            | _ -> None

    /// LSTNUT:LSTN_RELAY_ix_FUNCTION
    let lstn_relay_function (ix : int) (v : RelayFunction) : Characteristic = 
        let name = sprintf "LSTN_RELAY_%i_FUNCTION" ix
        _characteristic name (v.ToString())


    /// LSTNUT:LSTN_RELAY_ix_ON_LEVEL_M
    let lstn_relay_on_level (ix : int) (v : decimal) : Characteristic = 
        let name = sprintf "LSTN_RELAY_%i_ON_LEVEL_M" ix
        _characteristic name (sprintf "%M" v)

    /// LSTNUT:LSTN_RELAY_ix_OFF_LEVEL_M
    let lstn_relay_off_level (ix : int) (v : decimal) : Characteristic = 
        let name = sprintf "LSTN_RELAY_%i_OFF_LEVEL_M" ix
        _characteristic name (sprintf "%M" v)

    /// LSTNUT:LSTN_TRANSDUCER_MODEL
    let lstn_transducer_model (v : string) : Characteristic =         
        _characteristic "LSTN_TRANSDUCER_MODEL" v

    /// LSTNUT:LSTN_TRANSDUCER_SERIAL_NO
    let lstn_transducer_serial_no (v : string) : Characteristic =         
        _characteristic "LSTN_TRANSDUCER_SERIAL_NO" v



    