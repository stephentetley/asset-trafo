// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module CommonTypes =
    
    open System
   
    type ValuaValue = 
        | NullValue
        | IntValue of bigint
        | DecimalValue of decimal
        | TextValue of string
        override x.ToString() : string = 
            match x with
            | NullValue -> ""
            | IntValue i -> i.ToString()
            | DecimalValue d -> d.ToString()
            | TextValue s -> s

    let intValue (i : int) : ValuaValue = 
        IntValue (bigint i)

    let int32Value (i : int32) : ValuaValue = 
        IntValue (bigint i)

    let uint32Value (i : uint32) : ValuaValue = 
        IntValue (bigint i)

    let textUpperCase (s : string) : ValuaValue = 
        TextValue <| s.ToUpper().Trim()

    let valueFromOutput (v : ValuaValue) : string = 
        match v with
        | NullValue -> ""
        | IntValue i -> i.ToString()
        | DecimalValue d -> d.ToString()
        | TextValue s -> ""

    let valueToOutput (v : ValuaValue) : string = 
        match v with
        | NullValue -> ""
        | IntValue i -> "0"
        | DecimalValue d -> "0"
        | TextValue s -> ""
    
    let characteristicValueOutput (v : ValuaValue) : string = 
        match v with
        | NullValue -> ""
        | IntValue i -> ""
        | DecimalValue d -> ""
        | TextValue s -> s

    let dateDefault : DateTime = 
        new DateTime(year = 1970, month = 1, day = 1)
       
