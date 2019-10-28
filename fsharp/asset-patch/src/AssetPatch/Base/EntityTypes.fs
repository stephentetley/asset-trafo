// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base



module EntityTypes =
    
    open AssetPatch.Base
    open AssetPatch.Base.FuncLocPath
   
    type FuncLocSegment = 
      { Name : string
        Description : string
        ObjectType: string 
      }



    // The other way is to look at differences to an existing root funcloc
    // Then only 8 fields change:
    //
    // 1   FUNCLOC
    // 2   TXTMI
    // 38  FLTYP
    // 42  IEQUI
    // 56  FLOC_REF  {- Magic -}
    // 62  EQART
    // 63  JOBJN_FL  {- Magic -}
    // 94  TPLMA1
    // 95  TPLMA
    

    type FuncLoc = 
      { Path : FuncLocPath
        Description : string
        ObjectType : string
        Attributes : AssocList<string, string>
      }
        member x.Level with get () : int = x.Path.Level

        static member Initial (attributes : AssocList<string, string>) : FuncLoc option = 
            match AssocList.tryFind "FUNCLOC" attributes, 
                    AssocList.tryFind "TXTMI" attributes, 
                    AssocList.tryFind "EQART" attributes with
            | Some funcloc, Some desc, Some otype -> 
                Some { Path = FuncLocPath.Create funcloc 
                       Description = desc
                       ObjectType = otype
                       Attributes = attributes
                      }
             | _,_,_ -> None

    let extendFuncLoc (segment : FuncLocSegment) 
                      (floc: FuncLoc) : FuncLoc = 
        { Path = FuncLocPath.extend segment.Name floc.Path
          Description = segment.Description
          ObjectType = segment.Description
          Attributes = floc.Attributes }
