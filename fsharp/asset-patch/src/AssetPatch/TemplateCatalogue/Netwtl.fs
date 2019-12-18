// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplateCatalogue



module Netwtl =
    
    open AssetPatch.TemplatePatcher.CommonTypes
    open AssetPatch.TemplatePatcher.Template


    /// Class:NETWTL
    let netwtl : Characteristic list -> Class = 
        _class "NETWTL" 
    
    /// NETW_SUPPLY_VOLTAGE
    let netw_supply_voltage (v : int) : Characteristic =         
        _characteristic "NETW_SUPPLY_VOLTAGE" (intValue v)

    /// NETW_SUPPLY_VOLTAGE_UNITS
    let netw_supply_voltage_units (v : string) : Characteristic =         
        _characteristic "NETW_SUPPLY_VOLTAGE_UNITS" (TextValue v)

    /// IP_RATING
    let ip_rating (v : string) : Characteristic =         
        _characteristic "IP_RATING" (TextValue v)

    /// NETW_SPECIFIC_ORDER_CODE
    let netw_specific_order_code (v : string) : Characteristic =         
        _characteristic "NETW_SPECIFIC_ORDER_CODE" (TextValue v)
    
    /// MEMO_LINE
    let memo_line (v : string) : Characteristic =         
        _characteristic "MEMO_LINE" (TextValue v)

    /// LOCATION_ON_SITE
    let location_on_site (v : string) : Characteristic =         
        _characteristic "LOCATION_ON_SITE" (TextValue v)

    /// SPECIFIC_MODEL
    let specific_model (v : string) : Characteristic =         
        _characteristic "SPECIFIC_MODEL" (TextValue v)

    /// MANUFACTURERS_ASSET_LIFE_YR
    let manufacturers_asset_life_yr (v : int) : Characteristic =         
        _characteristic "MANUFACTURERS_ASSET_LIFE_YR" (intValue v)
