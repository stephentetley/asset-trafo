// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Utilities


module TidyChangeFile = 
    
    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.Parser
    open AssetPatch.Base.Printer

    let tidyChangeFile (priorities : string list) 
                        (removes : string list)
                        (sourceFile: string)
                        (destFile : string) : Result<unit, ErrMsg> =
        let transform = 
            AbsPatch.ofPatchFile 
                >> AbsPatch.prioritize priorities 
                >> AbsPatch.restrict removes
                >> AbsPatch.toPatchFile
        try 
            readChangeFile sourceFile
                |> Result.bind transform
                |> Result.map (writeChangeFile destFile)
        with
        | ex -> Error (ex.Message)
    