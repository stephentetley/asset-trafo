// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause



namespace AssetTrafo.Aib


module HKey = 

    // ************************************************************************
    // HKeys

    let hkeySubkey (start : int) (len : int) (hkey : string) : string option =
        try 
            hkey.[start .. start + (len - 1)] |> Some
        with
        | _ -> None


    let hkeyBusinessUnit (hkey : string) : string option = 
        hkeySubkey 0 1 hkey

    let hkeySystem (hkey : string) : string option = 
        hkeySubkey 1 3 hkey

    let hkeyFunction (hkey : string) : string option = 
        hkeySubkey 4 4 hkey

    let hkeyInstallation (hkey : string) : string option = 
        hkeySubkey 8 5 hkey
     
    let hkeySubInstallation (hkey : string) : string option = 
        hkeySubkey 13 5 hkey

    let hkeyProcessGroup (hkey : string) : string option = 
        hkeySubkey 18 2 hkey

    let hkeyProcess (hkey : string) : string option = 
        hkeySubkey 20 4 hkey

    let isAnon (hkeyFragment : string) : bool =
        hkeyFragment.Length > 0 && String.forall (fun ch -> ch = 'X') hkeyFragment
        