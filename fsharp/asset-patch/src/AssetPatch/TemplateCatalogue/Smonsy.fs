// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplateCatalogue


[<AutoOpen>]
module Lstnut =
    
    open AssetPatch.TemplatePatcher.Template

    
    

    /// Class:SMONSY
    let smonsy : Characteristic list -> Class = 
        _class "SMONSY" 
    

    /// SYSTEM_TYPE
    let system_type (v : string) : Characteristic = 
        _characteristic "SYSTEM_TYPE" v
