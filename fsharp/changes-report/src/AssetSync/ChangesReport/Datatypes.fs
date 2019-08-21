// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.ChangesReport


module Datatypes =
    
    /// Assets have "properties" { name, common name, grid ref... }
    /// that are stored inline with the "Asset record". 
    /// Because they are stored inline, "properties" are diffferent
    /// to attributes that are stored in a separate table with a foreign
    /// key back to the asset.
    /// The input report lists both AIDE an AI property values regardless
    /// of whether the value has changed in AIDE.
    type AssetProperty = 
        { PropertyName : string
          AiValue : string
          AideValue : string
        }
        member x.HasChanged with get () : bool = x.AideValue <> x.AiValue