// Copyright (c) Stephen Tetley 2019

#r "netstandard"
open System

#load "..\src\AssetSync\ChangeRequest\Syntax.fs"
#load "..\src\AssetSync\ChangeRequest\FlocMonad.fs"
#load "..\src\AssetSync\ChangeRequest\ChangeRequest.fs"
open AssetSync.ChangeRequest.FlocMonad
open AssetSync.ChangeRequest

let demo01 () = 
    execFlocMonad { StartUpDate = DateTime(year =2019, month=07, day=02) }
        <| flocBuilder {
                let! floc1 = 
                    root "BEW03" 
                            >>= extend "EDG" 
                            >>= extend "LQD" 
                            >>= extendx "SYS01" "EA Monitoring System"
                let! monitor = addEquipment "Level Monitor 1" 1000010UL floc1 
                return ()
            }
