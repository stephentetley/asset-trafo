// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Xml.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat"
open SLFormat.CommandOptions

#I @"C:\Users\stephen\.nuget\packages\slpotassco\1.0.0-alpha-20190722b\lib\netstandard2.0"
#r "SLPotassco"
open SLPotassco.Potassco.Invoke

let demoDirectory () = 
    @"E:\coding\_other\potassco"

    

let demo01 () = 
    let demoDir = demoDirectory () 
    clingo demoDir None [ literal "--version"] []


let demo02 () = 
    let demoDir = demoDirectory ()
    clingo demoDir None [] ["toh_ins.lp"; "toh_enc.lp"]

let demo02b () = 
    let demoDir = demoDirectory ()
    runClingo demoDir None [] ["toh_ins.lp"; "toh_enc.lp"]

let demo03 () = 
    let demoDir = demoDirectory ()
    clingo demoDir (Some 0) [literal "--BAD"] ["toh_ins.lp"; "toh_enc.lp"]

let demo03b () = 
    let demoDir = demoDirectory ()
    runClingo demoDir (Some 0) [literal "--BAD"] ["toh_ins.lp"; "toh_enc.lp"]