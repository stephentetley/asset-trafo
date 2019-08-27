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

    type Diff1<'a> = 
        | InLeft of 'a
        | Match of 'a
        | Difference of left:'a * right:'a
        | InRight of 'a

        member v.SortKey 
            with get() : 'a = 
                match v with
                | InLeft s -> s
                | Match s -> s
                | Difference (s1,_) -> s1
                | InRight s -> s

    type Differences<'a> = Diff1<'a> list

    /// F# design guidelines say favour object-interfaces rather than records of functions...
    type IDiffComparer<'a, 'Key> = 
        abstract member GetKey : 'a -> 'Key
        abstract member ValueEquals: 'a -> 'a -> bool

    let stringHelper : IDiffComparer<string, string> = 
        { new IDiffComparer<string, string>
            with member __.GetKey s = s
                 member __.ValueEquals s1 s2 = s1 = s2 }


    /// Note - the inputs will be sorted.
    let diffLists (helper:IDiffComparer<'a, 'Key>) 
                  (leftList : 'a list) 
                  (rightList : 'a list) : Differences<'a> = 
        let rec work lefts rights cont = 
            match lefts,rights with
            | xs, [] -> cont (List.map InLeft xs)
            | [], ys -> cont (List.map InRight ys)
            | (x::xs, y::ys) ->                 
                match compare (helper.GetKey x) (helper.GetKey y) with
                | i when i = 0 -> 
                    work xs ys (fun ac -> 
                    if helper.ValueEquals x y then 
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
        work (List.sortBy helper.GetKey leftList) (List.sortBy helper.GetKey rightList) (fun x -> x)
            |> List.sortBy (fun x -> x.SortKey)

    let showDiffs (printer :'a -> string) (diffs : Diff1<'a> list) : string = 
        let sb = new StringBuilder ()
        let write1 (diff : Diff1<'a>) : unit = 
            match diff with
            | InLeft s -> sb.AppendLine ("-" + printer s) |> ignore
            | InRight s -> sb.AppendLine ("+" + printer s) |> ignore
            | Match s -> sb.AppendLine (" " + printer s) |> ignore
            
            | Difference(s1,s2) -> 
                sb.AppendLine (sprintf "*[%s]=>%s" (printer s1) (printer s2)) |> ignore
        List.iter write1 diffs
        sb.ToString()