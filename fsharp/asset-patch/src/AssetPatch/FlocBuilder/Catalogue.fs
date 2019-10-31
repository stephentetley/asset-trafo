// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.FlocBuilder



module Catalogue =
    
    open AssetPatch.FlocBuilder.Hierarchy
    
    let east_north : Characteristic list -> Class = 
        _class "EAST_NORTH" 378u 

    let easting (v : int) : Characteristic = 
        _char "EASTING" (v.ToString())

    let northing (v : int) : Characteristic = 
        _char "NORTHING" (v.ToString())


    let lstn_level_transmitter (name : string) : Class list -> Equipment = 
        _equipment name "LSTN"