// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetSync.StructureRelationships


module StructureItemDiff =
    
    open System.Text

    open AssetSync.StructureRelationships.Datatypes

    // StructureItem has quite a complicated diffing 
    // To find name changes we have to traverse the list ordered by SAI / PLI
    // To output we want an almost lexigraphical order but with '/' favoured over ' '


    type Diff1 = 
        | InLeft of StructureItem
        | Match of StructureItem
        | Difference of left:StructureItem * right:StructureItem
        | InRight of StructureItem

        member v.PathKey 
            with get() : string  = 
                match v with
                | InLeft s -> s.CommonName.Replace(' ', '?')
                | Match s -> s.CommonName.Replace(' ', '?')
                | Difference (s1,_) -> s1.CommonName.Replace(' ', '?')
                | InRight s -> s.CommonName.Replace(' ', '?')


    type Differences = Diff1 list

    /// F# design guidelines say favour object-interfaces rather than records of functions..

    /// Note - the inputs will be sorted before the differences
    /// are calculated. 
    /// We rely on the inputs being ordered for the algorithm 
    /// to work - figuratively a pointer is running through each 
    /// list, and one will pause if it gets ahead of the other.
    let diffLists (leftList : StructureItem list) 
                  (rightList : StructureItem list) : Differences =
        let refSort (xs : StructureItem list) = 
            xs |> List.sortBy (fun x -> x.Reference)

        let pathSort (xs : Diff1 list) = 
            xs |> List.sortBy (fun x -> x.PathKey)

        let rec work lefts rights cont = 
            match lefts,rights with
            | xs, [] -> cont (List.map InLeft xs)
            | [], ys -> cont (List.map InRight ys)
            | (x::xs, y::ys) ->                 
                match compare (x.Reference) (y.Reference) with
                | i when i = 0 -> 
                    work xs ys (fun ac -> 
                    if x = y then 
                        cont (Match x :: ac)
                    else
                        cont (Difference(x,y) :: ac))

                | i when i < 0 -> 
                    // x is not in (y::ys)                 
                    work xs rights (fun ac -> 
                    cont (InLeft x :: ac))
                | i when i > 0 -> 
                    // y is not in (x::xs)
                    work lefts ys (fun ac -> 
                    cont (InRight y :: ac))
                | i -> failwithf "differenceL - Weird (impossible) pattern failure: %i" i
        work (refSort leftList) (refSort rightList) (fun x -> x)
            |> pathSort 

    let showDiffs (printer :StructureItem -> string) (diffs : Diff1 list) : string = 
        let sb = new StringBuilder ()
        let write1 (diff : Diff1) : unit = 
            match diff with
            | InLeft s -> sb.AppendLine ("-" + printer s) |> ignore
            | InRight s -> sb.AppendLine ("+" + printer s) |> ignore
            | Match s -> sb.AppendLine (" " + printer s) |> ignore
            
            | Difference(s1,s2) -> 
                sb.AppendLine (sprintf "*[%s]=>%s" (printer s1) (printer s2)) |> ignore
        List.iter write1 diffs
        sb.ToString()