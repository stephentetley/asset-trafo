// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplateCatalogue



module Ctossy =
    
    open AssetPatch.TemplatePatcher.CommonTypes
    open AssetPatch.TemplatePatcher.Template


    /// Class:CTOSSY
    let ctossy : Characteristic list -> Class = 
        _class "CTOSSY" 
    

    /// SYSTEM_TYPE
    /// Often "REMOTE TELEMETRY OUTSTATION"...
    let system_type (v : string) : Characteristic = 
        _characteristic "SYSTEM_TYPE" (TextValue v)

        
