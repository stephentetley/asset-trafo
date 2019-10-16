// Copyright (c) Stephen Tetley 2019

#r "netstandard"

#load "..\src\AssetSync\ChangeRequest\Syntax.fs"
#load "..\src\AssetSync\ChangeRequest\FlocMonad.fs"
#load "..\src\AssetSync\ChangeRequest\ChangeRequest.fs"
open AssetSync.ChangeRequest.FlocMonad
open AssetSync.ChangeRequest

let demo01 () = 
    execFlocMonad 
        <| flocBuilder {
                let! newFloc = root "BEW03" >>= extend "EDG" >>= extend "LQD"
                let! monitor = addEquipment "Level Monitor 1" 1000010UL newFloc 
                return 1
            }
