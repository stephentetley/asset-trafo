// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Base


module FactsCommon =

    open System.IO

    open FactX
    open FactX.FactWriter

    type ErrMsg = string

    let generatePredicates (makePred : 'row -> Predicate option) 
                            (source : 'row list) : FactWriter<unit> =
        List.choose makePred source
            |> List.distinct
            |> List.sort
            |> mapMz tellPredicate

    let writeFactsWithHeaderComment (outputFile : string)
                                    (factOutput: FactWriter<unit>) : unit =
        let shortName = Path.GetFileName outputFile
        let audit = sprintf "Generated: %s" (System.DateTime.Now.ToString(format = "yyyy-MM-dd HH:mm:ss"))
        runFactWriter 160 outputFile 
            <| factWriter { 
                    do! tellComment shortName
                    do! tellComment audit
                    do! factOutput
                    return ()
                }

 