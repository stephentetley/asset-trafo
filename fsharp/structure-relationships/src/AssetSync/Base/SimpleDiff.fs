// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.Base


module SimpleDiff =
    
    open System.Text

    // Maybe for SLAlignment?

    // Powershell's diff (compare-object) is not very good
    // and we cannot invoke Gnu diff via Cygwin, this is a 
    // simple alternative relaying on the fact that our input
    // is ordered.

    type Diff1 = 
        | InLeft of string
        | LineMatch of string
        | InRight of string

    type Differences = Diff1 list

    /// Relies on the inputs being ordered.
    let diffLists (leftList : string list) (rightList : string list) : Diff1 list = 
        let rec work lefts rights cont = 
            match lefts,rights with
            | xs, [] -> cont (List.map InLeft xs)
            | [], ys -> cont (List.map InRight ys)
            | (x::xs, y::ys) -> 
                match compare x y with
                | i when i = 0 -> 
                    work xs ys (fun ac -> 
                    cont (LineMatch x :: ac))
                | i when i < 0 -> 
                    // x is not in (y::ys)                 
                    work xs rights (fun ac -> 
                    cont (InLeft x :: ac))
                | i when i > 0 -> 
                    // y is not in (x::xs)
                    work lefts ys (fun ac -> 
                    cont (InRight y :: ac))
                | i -> failwithf "differenceL - Weird (impossible) pattern failure: %i" i
        work leftList rightList (fun x -> x)

    let showDiffs (diffs : Diff1 list) : string = 
        let sb = new StringBuilder ()
        let write1 (diff : Diff1) : unit = 
            match diff with
            | InLeft s -> sb.AppendLine ("-" + s) |> ignore
            | InRight s -> sb.AppendLine ("+" + s) |> ignore
            | LineMatch s -> sb.AppendLine (" " + s) |> ignore
        List.iter write1 diffs
        sb.ToString()