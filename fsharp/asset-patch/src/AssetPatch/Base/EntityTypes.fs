// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base



module EntityTypes =
    
    open AssetPatch.Base
    open AssetPatch.Base.FuncLocPath
   
    type FuncLoc = 
      { Path : FuncLocPath
        Description : string
        ObjectType : string
        Attributes : AssocList<string, string>
      }