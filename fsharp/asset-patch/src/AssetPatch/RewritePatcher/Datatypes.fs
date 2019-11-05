// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher



module Datatypes =
    
    open AssetPatch.Base.FuncLocPath

    // Flat or recursive??
    // Try flat for now...

    type Characteristic = 
        { Name : string
          Value : string 
        }

    type Class = 
        { ClassName : string          
          ClassInt : uint32
          Characteritics : Characteristic list
        }



    type Floc = 
        { Path : FuncLocPath 
          Description : string
          ObjectType : string
          Classes : Class list
        }

        member x.Level 
            with get() : int = x.Path.Level
