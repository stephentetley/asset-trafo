// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.AspFacts


module Common =

    open FactX
    open FactX.FactWriter

    let generatePredicates (makePred : 'row -> Predicate option) 
                            (source : 'row list) : FactWriter<unit> =
        List.choose makePred source
            |> List.distinct
            |> mapMz tellPredicate


