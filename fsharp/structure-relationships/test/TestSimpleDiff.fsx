// Copyright (c) Stephen Tetley 2019

#r "netstandard"
open System.IO

#load "..\src\AssetSync\Base\SimpleDiff.fs"
open AssetSync.Base.SimpleDiff

let outputFile (relativePath : string) = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\output\", relativePath)

let test01 () = 
    let contents1 = File.ReadAllLines(outputFile "ai2.csv") |> Array.toList
    let contents2 = File.ReadAllLines(outputFile "ai2.csv") |> Array.toList
    diffLists contents1 contents2


let test02 () = 
    let contents1 = ["apple"; "banana"; "grapefruit"; "orange"; "pear"]
    let contents2 = ["banana"; "grapefruit"; "lime"; "lemon"; "pear"]
    diffLists contents1 contents2 |> showDiffs |> printfn "%s"
