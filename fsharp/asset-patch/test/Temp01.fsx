// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Text.Encoding.dll"
open System.IO

let list01 () : int list = 
    [
      yield 1
      yield 2
    ]

let list02 () : int list = 
    let x = 0
    [
      if x = 0 then yield 1
      yield 2
    ]

let list03 () : int list = 
    let x = 0
    [
      if x <> 0 then yield 1
      yield 2
    ]



