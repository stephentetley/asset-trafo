// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FuncLocBuilder

module FuncLoc =

    open AssetPatch.Base.Syntax

    /// Store the precedence (to order how rows are printed) and 
    /// column name meta data along with the value.


    type Field = 
        | Field of precedence : int * fieldName : string * value : string

    let funcloc (value : string) : Field = 
        Field (1, "FUNCLOC", value)

    let txtmi (value : string) : Field = 
        Field (2, "TXTMI", value)

    /// This is ``superior functloc`` but we wouldn't know it 
    /// from the code...
    let tplma (value : string) : Field = 
        Field (2, "TPLMA", value)


    /// The other way is to look at differences to an existing root funcloc
    /// Then only 8 fields change:
    ///
    /// 1   FUNCLOC
    /// 2   TXTMI
    /// 38  FLTYP
    /// 56  FLOC_REF  {- Magic -}
    /// 62  EQART
    /// 63  JOBJN_FL  {- Magic -}
    /// 94  TPLMA1
    /// 95  TPLMA
    
    
    